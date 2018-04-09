(ns headless.core-test
  (:require [headless.core :as core]
            [clojure.test :refer :all]))

(use-fixtures :once
  (fn [test] (core/with-browser (test))))

(deftest connect-test
  (let [connection (core/connect {:host "localhost" :port 9222})]
    (is (= [:client :command-id :properties :command-promises :event-handlers]
           (keys connection)))))

(deftest execute-test
  (let [connection (core/connect {:host "localhost" :port 9222})
        document @(core/execute connection "DOM.getDocument" {})]
    (is (integer? (-> document :root :nodeId)))
    (is (= (-> document :root :baseURL) "about:blank"))))

(deftest register-event-handler-test
  (let [events (atom [])
        connection (core/connect {:host "localhost" :port 9222})
        handler (fn [event] (swap! events conj event))
        deregister (core/register-event-handler
                    connection "Page.frameStartedLoading" handler)]

    (testing "register"
      @(core/execute
        connection "Page.navigate" {:url "http://localhost:9222"})
      (Thread/sleep 10)
      (is (= (count @events) 1))
      (is (= (keys (first @events)) [:frameId])))

    (testing "deregister"
      (deregister)
      @(core/execute
        connection "Page.navigate" {:url "http://localhost:9222"})
      (Thread/sleep 10)
      (is (= (count @events) 1)))))
