(ns headless.core-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer :all]
            [headless.core :as core]))

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

(deftest select-test
  (let [connection (core/connect {:host "localhost" :port 9222})
        url "http://localhost:9222/"]
    (core/visit connection url)
    (let [node-ids (core/select connection "head,body")]
      (is (= (count node-ids) 2)))))

(deftest select-one-test
  (let [connection (core/connect {:host "localhost" :port 9222})
        url "http://localhost:9222/"]
    (core/visit connection url)
    (let [node-id (core/select-one connection "head")]
      (is (integer? node-id)))))

(deftest evaluate-test
  (let [connection (core/connect {:host "localhost" :port 9222})
        url "http://localhost:9222/"]
    (core/visit connection url)
    (let [title-node-id (core/select-one connection "title")]
      (is (= (core/evaluate connection title-node-id
                            "function() { return 1 + 1; }")
             2))
      (is (= (core/evaluate connection title-node-id
                            "function() { return {x: 42}; }")
             {:x 42}))
      (is (= (core/evaluate connection title-node-id
                            "function() { return this.innerText; }")
             "Headless remote debugging")))))

(deftest text-test
  (let [connection (core/connect {:host "localhost" :port 9222})
        url "http://localhost:9222/"]
    (core/visit connection url)
    (let [node-id (core/select-one connection "title")
          text (core/text connection node-id)]
      (is (= text "Headless remote debugging")))))

(deftest html-test
  (let [connection (core/connect {:host "localhost" :port 9222})
        url "http://localhost:9222/"]
    (core/visit connection url)
    (let [node-id (core/select-one connection "title")
          html (core/html connection node-id)]
      (is (= html "<title>Headless remote debugging</title>")))))

;; TODO implement tests that require a more extensive html page and possibly
;; even network to be tested. The interception stuff i will implement for
;; caching should solve this problem.
(deftest click-test)
(deftest attributes-test)
(deftest attribute-test)
(deftest evaluate-test)
(deftest with-await-event-test)
(deftest visit-test)
