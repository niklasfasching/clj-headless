(ns headless.cache
  "HTTP response cache that automatically makes you a better person.
  During development one often ends up fetching the same pages many times in a
  row. To be a good (less bad) citizen and not be too much a nuisance to the
  page owners caching enabled in those situations.

  Luckily, the debugging protocol allows intercepting and manually answering
  network requests. Unluckily, it is not straightforward to implement a cache
  based on that. Intercepted requests are not linked to a request-id but an
  interception-id. Response headers are linked to request-id but not
  interception-id and response headers are linked to both.
  In the end, all that is required is some boilerplate to link those parts
  anyways and a firm belief that requests are truly uniquely identified through
  the tuple [url method body]."
  (:refer-clojure :exclude [get contains?])
  (:require [headless.core :as core]
            [clojure.string :as string])
  (:import (java.util Base64 LinkedHashMap)))

(def ^:dynamic *max-concurrent-requests*
  "Maximum concurrently active requests. Determines the amount of request keys
  and response heads we need to keep around to stitch together the responses."
  100)

(def base64-encoder (Base64/getEncoder))

(def base64-decoder (Base64/getDecoder))

(defprotocol Cache
  (put [this key value])
  (get [this value])
  (contains? [this key]))

(extend-type LinkedHashMap
  Cache
  (put [this key value] (.put this key value))
  (get [this key] (.get this key))
  (contains? [this key] (.containsKey this key)))

(defn fifo-cache
  "Returns a mutable fifo map limted to `max-size` entries. Implements `Cache`."
  [max-size]
  (proxy [LinkedHashMap] []
    (removeEldestEntry [_] (> (.size this) max-size))))

(defn request-key
  "Returns unique key for `request`. Hash of url, method & postData."
  [{:keys [url method postData] :as request}]
  (hash [url method postData]))

(defn http-head
  "Returns http head (status line + headers). ~ http response without body."
  [{:keys [headers status statusText] :as response}]
  (let [status-line (format "HTTP/1.1 %s %s" status statusText)
        header-lines (for [[k v] headers] (format "%s: %s" (name k) v))]
    (string/join "\r\n" (concat [status-line] header-lines ["\r\n"]))))

(defn on-request
  "Add unique request-key for request from `event` to cache.
  The key for the request is stored on the request-id and is used to link
  intercepted requests (request-key), response heads (request-id) and response
  bodies (request-id)."
  [{:keys [properties] :as connection} {:keys [requestId request] :as event}]
  (let [{:keys [cache/requests
                cache/responses]} @properties
        key (request-key request)]
    (put requests requestId key)))

(defn on-response
  "Add response head (status-line + headers) of response from `event` to cache.
  The response head is stored on the request-key (as request-ids are unique
  and change from call to call)."
  [{:keys [properties] :as connection} {:keys [requestId response] :as event}]
  (let [{:keys [cache/requests
                cache/response-heads
                cache/responses]} @properties
        request-key (get requests requestId)]
    (when-not (contains? responses request-key)
      (put response-heads request-key (http-head response)))))

(defn on-loaded
  "Add full response (head + body) for requestId from `event` to cache.
  Receiving this event notifies us that we can now fetch the response body using
  `core/network-get-response-body`. The body retrieved for the `requestId` is
  linked with the response head via the request key and stored in the cache."
  [{:keys [properties] :as connection} {:keys [requestId] :as event}]
  (let [{:keys [cache/requests
                cache/responses
                cache/response-heads]} @properties
        request-key (get requests requestId)]
    (when-not (contains? responses request-key)
      (let [{:keys [base64Encoded body]} (core/network-get-response-body
                                          connection :requestId requestId)
            head-bytes (->> request-key (get response-heads) .getBytes)
            body-bytes (if base64Encoded
                         (.decode base64-decoder body)
                         (.getBytes body))
            response-base64 (->> (concat head-bytes body-bytes)
                                 (byte-array)
                                 (.encodeToString base64-encoder))]
        (put responses request-key response-base64)))))

(defn on-intercepted
  "Handle intercepted request. Serves response from cache when possible."
  [connection {:keys [interceptionId request] :as event}]
  (let [properties (:properties connection)
        {:keys [cache/responses]} @properties
        response-base64 (get responses (request-key request))]
    (if response-base64
      (core/network-continue-intercepted-request
       connection :interceptionId interceptionId :rawResponse response-base64)
      (core/network-continue-intercepted-request
       connection :interceptionId interceptionId))))

(defn register-handlers
  "Registers handlers for events. Returns zero-arity fn to deregister handlers."
  [connection]
  (let [handlers {core/network-on-request-will-be-sent on-request
                  core/network-on-response-received on-response
                  core/network-on-loading-finished on-loaded
                  core/network-on-request-intercepted on-intercepted}
        deregister-fs (mapv (fn [[on-event handler]]
                              (on-event connection #(handler connection %)))
                            handlers)]
    #(doseq [f deregister-fs] (f))))

(defn disable
  "Remove cache from `connection` and deregister event handlers."
  [{:keys [properties] :as connection}]
  (let [deregister (:cache/deregister @properties)]
    (swap! properties dissoc
           :cache/requests
           :cache/response-heads
           :cache/responses
           :cache/deregister)
    (if deregister (deregister))))

(defn enable
  "Enable response cache for `connection` with `response-cache` as storage.
  `response-cache` must implement the `Cache` protocol. Defaults to fifo-cache
  with 500 entries.
  This features makes use of the experimental request interception support of
  the debugging protocol. Multiple other handlers are required as intercepted
  requests and responses can only be linked indirectly via the request object.
  See `request-key` and the `on-*` functions in this namespace for more info."
  ([connection] (enable connection (fifo-cache 500)))
  ([connection response-cache]
   (disable connection)
   (swap! (:properties connection) assoc
          :cache/requests (fifo-cache *max-concurrent-requests*)
          :cache/response-heads (fifo-cache *max-concurrent-requests*)
          :cache/responses response-cache
          :cache/deregister (register-handlers connection))
   (core/network-set-request-interception
    connection :patterns [{:urlPattern "*"}])))
