(ns headless.util
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [yawc.core :as yawc])
  (:import (java.util.concurrent Executors)
           (java.net Socket)))

(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [this thread exception]
     (throw exception))))

(def event-handler-executor
  "Event handlers are called within the message callback of the websocket
  client. This callback is executed synchrously and blocking inside it (e.g. to
  await a command reply) means blocking consumption of further messages. We have
  to execute handlers asynchronously and can't use futures as those swallow
  exceptions unless dereffed. Exceptions are rethrown using
  `UncaughtExceptionHandler`."
  (Executors/newSingleThreadExecutor))

(def timeout-error {:error {:timeout ::*timeout*}})

(def ^:dynamic *timeout*
  "Maximum time to wait for browser in ms."
  10000)

(def ^:dynamic *browser-args*
  "Map used to build arguments to Runtime.exec in `with-browser`.
  Relevant keys [:executable :port :headless :args]. :args in appended to the
  generated arguments."
  {:executable "chromium-browser" :port 9222 :headless true :args []})

(defn execute
  "Send command to yawc-`client`. Either returns the result or throws the error.
  - `method`: string name of the method, e.g. 'DOM.getDocument'.
  - `params`: map of parameters for the given `method`."
  [{:keys [command-id command-promises client] :as connection} method params]
  (let [id (swap! command-id inc)
        promised (promise)
        payload (-> {:id id :method method :params params}
                    json/write-str .getBytes)]
    (swap! command-promises assoc id promised)
    (yawc/emit client {:fin 1 :opcode 1 :payload payload})
    (let [{:keys [result error]} (deref promised *timeout* timeout-error)]
      (or result (throw (ex-info (str method " " params) error))))))

(defn handlers-for-domain?
  "Check whether `event-handlers` contains any handlers for `domain-name`."
  [event-handlers domain-name]
  (->> event-handlers
       (some (fn [[event-name handlers]]
               (and (string/starts-with? event-name domain-name)
                    (not-empty handlers))))
       (boolean)))

(defn domain-name [event-name]
  (-> event-name (string/split #"[.]") first))

(defn deregister-event-handler
  "Deregisters `handler` for `event-name` in `event-handlers`.
   Automatically unsubscribes from events for domain if no other handlers are
  registered for it."
  [{:keys [event-handlers] :as connection} event-name handler]
  (let [domain (domain-name event-name)]
    (swap! event-handlers update-in [event-name] #(vec (remove #{handler} %)))
    (when-not (handlers-for-domain? @event-handlers domain)
      (execute connection (str domain ".disable") {}))))

(defn register-event-handler
  "Registers `handler` for `event-name` in `event-handlers`.
  Returns a zero-arity function that deregisters the registered `handler`.
  Automatically subscribes to events for domain if no other handlers are
  already registered for it."
  [{:keys [event-handlers] :as connection} event-name handler]
  (let [domain (domain-name event-name)]
    (when-not (handlers-for-domain? @event-handlers domain)
      (execute connection (str domain ".enable") {}))
    (swap! event-handlers update-in [event-name] #(vec (conj % handler)))
    (partial deregister-event-handler connection event-name handler)))

(defn register-default-event-handlers
  "Registers some useful handlers in `event-handlers`.
  - Update root-node-id on `page-on-frame-stopped-loading`.
    Whenever the document is updated (or is fetched via `dom-get-document`) all
    existing node-ids become invalid, so we have to cache it.
    Listenig to `page-on-frame-stopped-loading` as that's what the visit fns
    listen on and `dom-on-document-updated` somehow fires multiple times."
  [{:keys [properties event-handlers] :as connection}]
  (let [root-node-id (fn [event]
                       (->> (execute connection "DOM.getDocument" {})
                            :root
                            :nodeId
                            (swap! properties assoc :root-node-id)))]
    (register-event-handler connection "Page.frameStoppedLoading" root-node-id)))

(defn open-page
  "Opens a new page (~= tab) at `host`:`port`. Returns the page-id."
  [{:keys [host port]}]
  (-> (format "http://%s:%s/json/new" host port)
      (slurp)
      (json/read-str :key-fn keyword)
      (:id)))

(defn handle-response
  "Forward `response` to `event-handlers` or `command-promises` based on type.
  - responses with :method and :params are events. The value of :params is sent
    asynchronously to to all registered `event-handlers`.
  - responses with an :id are commands. The complete response with :error, :id &
    :result is delivered to the promise with the corresponding command-id."
  [command-promises event-handlers {:keys [method params id] :as response}]
  (cond method (doseq [handler (get @event-handlers method)]
                 (.execute event-handler-executor #(handler params)))
        id (do (deliver (get @command-promises id) response)
               (swap! command-promises dissoc id))
        :else (throw (ex-info "Invalid response" response))))

(defn connect
  "Open a connection to page using `options`. Returns connection.
  Defaults to `host` localhost and `port` 9222. If `page-id` is nil, opens a new
  page using `open-page`.
  The returned connection map contains the following fields:
  - `client`: yawc client - the underlying websocket connection
  - `command-id`: an integer atom that is increased to create unique command
    ids. Command ids are used to associate received responses to sent commands.
  - `command-promises`: a map atom of `command-id` to promise. The promises are
    eventually resolved with their respective response from the browser. See
    `handle-response`.
  - `event-handlers`: a map atom of event-name (e.g. DOM.documentUpdated) to
    array of one-arity handler functions to call with the event. See
    `handle-response`.
  - `properties`: a map atom holding any additional information for the
     connection to allow generic customizing of the connection."
  [& {:keys [host port page-id] :as options}]
  (let [host (or host "localhost")
        port (or port 9222)
        page-id (or page-id (open-page {:host host :port port}))
        path (str "/devtools/page/" page-id)
        event-handlers (atom {})
        command-promises (atom {})
        command-id (atom 0)
        properties (atom {})
        cb (fn [type payload _]
             (when (= type :text)
               (->> (json/read-str payload :key-fn keyword)
                    (handle-response command-promises event-handlers))))
        client (yawc/open {:path path :host host :port port :cb cb})
        connection {:client client :command-id command-id :properties properties
                    :command-promises command-promises
                    :event-handlers event-handlers}]
    (register-default-event-handlers connection)
    connection))

(defmacro with-browser
  "Run `body` with browser running. Closes browser afterwards.
  Waits `*timeout*` ms for the browser to become ready to connect to the remote
  debugging port.
  Rebind `*browser-args*` map to customize how the browser is started."
  [& body]
  `(let [args# `[~(:executable *browser-args*)
                 ~(str "--remote-debugging-port=" (:port *browser-args*))
                 ~@(if (:headless *browser-args*)
                     ["--headless" "--disable-gpu"])]
         process# (.exec (Runtime/getRuntime) (into-array String args#))]
     (loop [wait-ms# 0]
       (when (try
               (.close (Socket. "localhost" (:port *browser-args*)))
               false
               (catch Exception e#
                 (< wait-ms# *timeout*)))
         (Thread/sleep 10)
         (recur (+ wait-ms# 10))))
     (try (do ~@body)
          (finally
            (.destroy process#)
            (.waitFor process#)))))

(defmacro defcopy
  "Copies `source-name` to `name` in the current namespace. Merges metadata.
  Importing multiple namespaces is annoying. This allows us to expose a few
  utility functions in the main namespace for usability.
  See https://groups.google.com/forum/#!msg/clojure/LhidPSlvX_Q/mS4R_52suXwJ."
  ([name source-name]
   `(alter-meta!
     (def ~name (.getRawRoot (var ~source-name)))
     conj (meta (var ~source-name)))))
