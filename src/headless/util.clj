(ns headless.util
  (:require [clojure.string :as string]
            [clojure.data.json :as json]))

(defn handlers-for-domain?
  "Check if `event-handlers` contains any handlers for `domain-name`."
  [event-handlers domain-name]
  (->> event-handlers
       (some (fn [[event-name handlers]]
               (and (string/starts-with? event-name domain-name)
                    (not-empty handlers))))
       (boolean)))

(defn domain-name [event-name]
  (-> event-name (string/split #"[.]") first))

(defn handle-response
  "Forward `response` to `event-handlers` or `command-promises`.
  A response is either an event or a reply to a command.
  Events specify a :method and are sent as-is (i.e. only the event, not the
  :method etc.) to all `event-handlers` registered for that method.
  Command replies contain an :id and are deliver the promise registered for that
  id in `command-promises` with {:error :result :id}."
  [command-promises event-handlers {:keys [method params id] :as response}]
  (cond
    method (doseq [handler (get @event-handlers method)]
             (handler params))
    id (let [p (get @command-promises id)]
         (deliver p response)
         (swap! command-promises dissoc id))
    :else (throw (ex-info "Invalid response" response))))

(defn open-page
  "Open new page for the headless instance at `host`:`port`. Returns a page-id.
  A page is a tab."
  [{:keys [host port]}]
  (-> (format "http://%s:%s/json/new" host port)
      (slurp)
      (json/read-str :key-fn keyword)
      (:id)))
