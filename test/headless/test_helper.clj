(ns headless.test-helper
  (:require [clojure.string :as string]
            [headless.cache :as cache]))

(defn base64-response
  "Returns base64 encoded response with `content` inside html body.
  See rawResponse from `core/network-continue-intercepted-request`."
  [content]
  (let [body (format "<html><body>%s</body></html>" content)
        lines ["HTTP/1.1 200 OK"
               (str "Content-Length: " (.length body))
               "" body]
        response (string/join "\r\n" lines)]
    (.encodeToString cache/base64-encoder (.getBytes response))))

(defn with-cache [connection cache-template]
  (let [cache (cache/fifo-cache (count cache-template))]
    (doseq [[url body] cache-template]
      (let [key (cache/request-key {:url url :method "GET"})
            response (base64-response body)]
        (cache/put cache key response)))
    (cache/enable connection cache)))
