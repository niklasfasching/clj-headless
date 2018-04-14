(ns headless.util-test
  (:require [clojure.test :refer :all]
            [headless.util :as util]))

(use-fixtures :once
  (fn [test] (util/with-browser (test))))

(deftest connect-test
  (let [connection (util/connect)]
    (is (= [:client :command-id :properties :command-promises :event-handlers]
           (keys connection)))))

(deftest execute-test
  (let [connection (util/connect)
        document (util/execute connection "DOM.getDocument" {})]
    (is (integer? (-> document :root :nodeId)))
    (is (= (-> document :root :baseURL) "about:blank"))))

(deftest register-event-handler-test
  (let [events (atom [])
        connection (util/connect)
        handler (fn [event] (swap! events conj event))
        deregister (util/register-event-handler
                    connection "Page.frameStartedLoading" handler)]

    (testing "register"
      (util/execute connection "Page.navigate" {:url "http://localhost:9222"})
      (Thread/sleep 10)
      (is (= (count @events) 1))
      (is (= (keys (first @events)) [:frameId])))

    (testing "deregister"
      (deregister)
      (util/execute connection "Page.navigate" {:url "http://localhost:9222"})
      (Thread/sleep 10)
      (is (= (count @events) 1)))))


(deftest domain-name-test
  (is (= (util/domain-name "DOM.querySelectorAll")
         "DOM"))
  (is (= (util/domain-name "Domain.method.with.dots")
         "Domain")))

(deftest handlers-for-domain?-test
  (let [handler (fn [event] nil)
        event-handlers {"DOM.attributeModified" [handler]
                        "Network.dataReceived" [handler]
                        "Page.frameNavigated" []}]

    (is (= (util/handlers-for-domain? event-handlers "DOM")
           true))
    (is (= (util/handlers-for-domain? event-handlers "Network")
           true))
    (is (= (util/handlers-for-domain? event-handlers "Page")
           false))
    (is (= (util/handlers-for-domain? event-handlers "Runtime")
           false))))

(deftest handle-response-test
  (let [handled-event1 (atom nil)
        handled-event2 (atom nil)
        promised (promise)
        event-handlers (atom {"Domain.Event" [#(reset! handled-event1 %)
                                              #(reset! handled-event2 %)]})
        command-promises (atom {1 promised})
        handle (partial util/handle-response command-promises event-handlers)]

    (testing "registered events are handled by all handlers"
      (handle {:method "Domain.Event" :params {:value "event"}})
      (Thread/sleep 10) ; event handlers are called asynchronously
      (is (= @handled-event1 {:value "event"}))
      (is (= @handled-event2 {:value "event"})))

    (testing "unregistered events are ignored"
      (handle {:method "Domain.OtherEvent" :params {:value "event"}}))

    (testing "command replies resolve the associated promise"
      (handle {:id 1 :result 42})
      (is (= @promised {:id 1 :result 42})))

    (testing "responses without either :id or :method throw"
      (is (thrown? Exception (handle {:result 42}))))))
