(ns headless.cache-test
  (:require [clojure.test :refer :all]
            [headless.cache :as cache]
            [headless.core :as core]
            [headless.test-helper :as test-helper]
            [clojure.string :as string]))

(use-fixtures :once
  (fn [test] (core/with-browser (test))))

(deftest request-key-test
  (let [get1 {:url "http://www.google.de" :method "GET" :headers {:x 2}}
        get2 {:url "http://www.google.de" :method "GET" :headers {:x 1}}
        post1 {:url "http://www.google.de" :method "POST" :postData "a=1"}
        post2 {:url "http://www.google.de" :method "POST" :postData "a=2"}]
    (is (= (cache/request-key get1)
           (cache/request-key get2)))
    (is (not= (cache/request-key get1)
              (cache/request-key post1)))
    (is (not= (cache/request-key post1)
              (cache/request-key post2)))))

(deftest fifo-cache-test
  (testing "mutable, size limited (fifo) map implementing the Cache protocol"
    (let [m (cache/fifo-cache 2)]
      (cache/put m 1 "1")
      (cache/put m 2 "2")
      (cache/put m 3 "3")
      (is (= (cache/contains? m 1) false))
      (is (= (cache/contains? m 2) true))
      (is (= (cache/contains? m 3) true))
      (cache/put m 4 "4")
      (is (= (cache/get m 2) nil))
      (is (= (cache/get m 3) "3"))
      (is (= (cache/get m 4) "4")))))

(deftest http-head-test
  (let [headers {:foo "foo" :bar "bar" :foo-bar "foo-bar"}
        response {:headers headers :status 200 :statusText "OK"}
        http-head (cache/http-head response)]
    (is (re-find #"HTTP/1\.1 200 OK" http-head))
    (is (re-find #"foo: foo" http-head))
    (is (re-find #"bar: bar" http-head))
    (is (re-find #"foo-bar: foo-bar" http-head))
    (is (string/ends-with? http-head "\r\n\r\n"))))

(deftest integration-test
  (testing "tries to respond from cache"
    (let [{:keys [properties] :as connection} (core/connect)
          url "http://www.example.localhost/"]
      (test-helper/with-cache connection {url "<p>foobar</p>"})
      (core/visit connection url)
      (is (= (->> (core/select-one connection "p")
                  (core/text connection))
             "foobar")))))

(deftest enable-test
  (testing "disables previous caches and associated handlers"
    (let [{:keys [event-handlers properties] :as connection} (core/connect)]
      (cache/enable connection)
      (cache/enable connection)
      (cache/enable connection)
      (is (= (count (get @event-handlers "Network.requestIntercepted"))
             1)))))

(deftest disable-test
  (let [{:keys [event-handlers properties] :as connection} (core/connect)]
    (cache/enable connection)
    (is (some? (:cache/responses @properties)))
    (is (= (count (get @event-handlers "Network.requestIntercepted"))
           1))
    (cache/disable connection)
    (is (nil? (:cache/responses @properties)))
    (is (= (count (get @event-handlers "Network.requestIntercepted"))
           0))))
