(ns headless.core
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [headless.generate :as generate]
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

(defmacro with-await-event
  "Execute `body` & await event from `register-event`. Returns result of `body`.
  Registers an event-handler for event using `register-event` and deregisters it
  afterwards."
  [connection register-event & body]
  `(let [event-promise# (promise)
         handler# (fn [~'event] (deliver event-promise# ~'event))
         deregister# (~register-event ~connection handler#)
         result# (do ~@body)]
     @event-promise#
     (deregister#)
     result#))

(generate/commands-and-events "devtools-protocol/json/browser_protocol.json")

(generate/commands-and-events "devtools-protocol/json/js_protocol.json")

(defn visit
  "Visits `url` on `connection`. Caches root-node-id in connection `properties`.
  To select elements, we need a root node - generally we want this to be
  the root-node of the document. As node-ids are recreated whenever
  the Page.getDocument method is called, we must cache the node-id of the
  document root. Otherwise we invalidate all node ids whenever we want to select
  an element (from the whole document)."
  [connection url]
  (let [result (with-await-event connection
                 page-on-frame-stopped-loading
                 @(page-navigate connection :url url))
        document @(dom-get-document connection)
        root-node-id (-> document :root :nodeId)]
    (swap! (:properties connection) assoc :root-node-id root-node-id)
    result))

(defn select-one
  "Select first element for css `selector` on `connection`. Returns node-id.
  Optionally, limit selection to children of `node-id` (defaults to document
  root)."
  ([connection selector]
   (select-one connection selector (:root-node-id @(:properties connection))))
  ([connection selector node-id]
   (let [{id :nodeId} @(dom-query-selector
                        connection :nodeId node-id :selector selector)]
     (if (= id 0) nil id))))

(defn select
  "Select elements for css `selector` on `connection`. Returns node-id array.
  Optionally, limit selection to children of `node-id` (defaults to document
  root)."
  ([connection selector]
   (select connection selector (:root-node-id @(:properties connection))))
  ([connection selector node-id]
   (let [result @(dom-query-selector-all
                  connection :nodeId node-id :selector selector)]
     (:nodeIds result))))

(defn evaluate
  "Evaluate `js-function` on `connection` with `node-id` bound to this.
  Throws exception when evaluation does not succeed.
  Example `js-function`: \"function() { return this.innerText; }\"."
  [connection node-id js-function]
  (let [objectId (-> @(dom-resolve-node connection :nodeId node-id)
                     :object :objectId)
        {:keys [result exceptionDetails]} @(runtime-call-function-on
                                            connection
                                            :returnByValue true
                                            :functionDeclaration js-function
                                            :objectId objectId)]
    (if exceptionDetails
      (throw (ex-info js-function exceptionDetails))
      (:value result))))

(defn html
  "Returns outer html of `node-id` from `connection`.
  The `dom-get-outer-html` method did not always work as expected (e.g. for
  title) so we do it the hard way."
  [connection node-id]
  (evaluate connection node-id "function() { return this.outerHTML; }"))

(defn text
  "Returns inner text of `node-id` from `connection`."
  [connection node-id]
  (evaluate connection node-id "function() { return this.innerText; }"))

(defn attributes
  "Returns attributes map of `node-id` from `connection`."
  [connection node-id]
  (->> @(dom-get-attributes connection :nodeId node-id)
       :attributes
       (apply assoc {})))

(defn attribute
  "Returns attribute with name `name` of `node-id` from `connection`."
  [connection node-id name]
  (-> (attributes connection node-id)
      (get name)))

(defn click
  "Scroll `node-id` from `connection` into view and click on it."
  [connection node-id]
  (->> "function() {
          this.scrollIntoViewIfNeeded();
          this.click();
        }"
       (evaluate connection node-id)))
