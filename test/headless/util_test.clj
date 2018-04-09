(ns headless.util-test
  (:require [headless.util :as util]
            [clojure.test :refer :all]))

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
      (is (= @handled-event1 {:value "event"}))
      (is (= @handled-event2 {:value "event"})))

    (testing "unregistered events are ignored"
      (handle {:method "Domain.OtherEvent" :params {:value "event"}}))

    (testing "command replies resolve the associated promise"
      (is (handle {:id 1 :result 42}))
      (is (= @promised {:id 1 :result 42})))

    (testing "responses without either :id or :method throw"
      (is (thrown? Exception (handle {:result 42}))))))
