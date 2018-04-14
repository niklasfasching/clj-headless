(ns headless.core
  (:require [clojure.data.json :as json]
            [headless.generate :as generate]
            [headless.util :as util]
            [yawc.core :as yawc]))

(load "core_protocol")

(util/defcopy connect util/connect)

(util/defcopy with-browser util/with-browser)

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

(defn visit
  "Visits `url` on `connection` and waits for page to stop loading."
  [connection url]
  (with-await-event connection
    page-on-frame-stopped-loading
    (page-navigate connection :url url)))

(defn select-one
  "Select first element for css `selector` on `connection`. Returns node-id.
  Optionally, limit selection to children of `node-id` (defaults to document
  root)."
  ([connection selector]
   (select-one connection selector (:root-node-id @(:properties connection))))
  ([connection selector node-id]
   (let [{id :nodeId} (dom-query-selector
                       connection :nodeId node-id :selector selector)]
     (if (= id 0) nil id))))

(defn select
  "Select elements for css `selector` on `connection`. Returns node-id array.
  Optionally, limit selection to children of `node-id` (defaults to document
  root)."
  ([connection selector]
   (select connection selector (:root-node-id @(:properties connection))))
  ([connection selector node-id]
   (let [result (dom-query-selector-all
                 connection :nodeId node-id :selector selector)]
     (:nodeIds result))))

(defn evaluate
  "Evaluate `js-function` on `connection` with `node-id` bound to this.
  Throws exception when evaluation does not succeed.
  Example `js-function`: \"function() { return this.innerText; }\"."
  [connection node-id js-function]
  (let [objectId (-> (dom-resolve-node connection :nodeId node-id)
                     :object :objectId)
        {:keys [result exceptionDetails]} (runtime-call-function-on
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
  (->> (dom-get-attributes connection :nodeId node-id)
       :attributes
       (apply assoc {})))

(defn attribute
  "Returns attribute with name `name` of `node-id` from `connection`."
  [connection name node-id]
  (-> (attributes connection node-id) (get name)))

(defn click
  "Scroll `node-id` from `connection` into view and click on it."
  [connection node-id]
  (->> "function() {
          this.scrollIntoViewIfNeeded();
          this.click();
        }"
       (evaluate connection node-id)))

(defn click-visit
  "Like `click`, but wait for navigation like `visit`."
  [connection node-id]
  (with-await-event connection
    page-on-frame-stopped-loading
    (click connection node-id)))
