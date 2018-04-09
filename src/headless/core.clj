(ns headless.core
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [headless.util :as util]
            [yawc.core :as yawc])
  (:import [java.net Socket]))

(def ^:dynamic *timeout*
  "Maximum amount of time to wait for browser in ms."
  10000)

(def timeout-error {:error {:timeout ::*timeout*}})

(def ^:dynamic *browser-args*
  "Map used to build arguments to Runtime.exec in `with-browser`.
  Relevant keys [:executable :port :headless :args]. :args in appended to the
  generated arguments."
  {:executable "chromium-browser" :port 9222 :headless true :args []})

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
     (try
       (do ~@body)
       (finally (.destroy process#)))))

(defn connect
  "Open a connection to page `page-id` at `host`:`port`. Returns connection.
  If `page-id` is nil, opens a new page using `util/open-page`.
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
  [{:keys [host port page-id] :as options}]
  (let [page-id (or page-id (util/open-page options))
        path (str "/devtools/page/" page-id)
        event-handlers (atom {})
        command-promises (atom {})
        command-id (atom 0)
        properties (atom {})
        cb (fn [type payload _]
             (when (= type :text)
               (->> (json/read-str payload :key-fn keyword)
                    (util/handle-response command-promises event-handlers))))
        client (yawc/open {:path path :host host :port port :cb cb})]
    {:client client :command-id command-id :properties properties
     :command-promises command-promises :event-handlers event-handlers}))

(defn execute
  "Send command to yawc-`client`. Returns a future for the eventual response.
  - `method`: string name of the method, e.g. \"DOM.getDocument\".
  - `params`: map of parameters for the given `method`.
  If the method call succeeds, the returned future resolves to the response.
  Otherwise, the received error is thrown."
  [{:keys [command-id command-promises client] :as connection} method params]
  (let [id (swap! command-id inc)
        p (promise)
        payload (-> {:id id :method method :params params}
                    json/write-str .getBytes)]
    (swap! command-promises assoc id p)
    (yawc/emit client {:fin 1 :opcode 1 :payload payload})
    (future (let [{:keys [result error]} (deref p *timeout* timeout-error)]
              (or result (throw (ex-info (str method " " params) error)))))))

(defn deregister-event-handler
  "Deregisters `handler` for `event-name` in `event-handlers`.
   Automatically unsubscribes from events for domain if no other handlers are
  registered for it."
  [{:keys [event-handlers] :as connection} event-name handler]
  (let [domain (util/domain-name event-name)]
    (swap! event-handlers update-in [event-name] #(remove #{handler} %))
    (when-not (util/handlers-for-domain? @event-handlers domain)
      @(execute connection (str domain ".disable") {}))))

(defn register-event-handler
  "Registers `handler` for `event-name` in `event-handlers`.
  Returns a zero-arity function that deregisters the registered `handler`.
  Automatically subscribes to events for domain if no other handlers are
  already registered for it."
  [{:keys [event-handlers] :as connection} event-name handler]
  (let [domain (util/domain-name event-name)]
    (when-not (util/handlers-for-domain? @event-handlers domain)
      @(execute connection (str domain ".enable") {}))
    (swap! event-handlers update-in [event-name] #(conj % handler))
    (partial deregister-event-handler connection event-name handler)))
