(ns headless.core-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer :all]
            [headless.core :as core]
            [headless.test-helper :as test-helper]))

(use-fixtures :once
  (fn [test] (core/with-browser (test))))

(deftest with-await-event-test
  (let [connection (core/connect)
        event (atom nil)
        url "http://www.example.localhost/"]
    (test-helper/with-cache connection {url ""})
    (core/page-on-frame-stopped-loading connection #(reset! event %))
    (core/with-await-event connection
      core/page-on-frame-stopped-loading
      (core/page-navigate connection :url url)
      (is (not (some? @event))))
    (is (some? @event))))

(deftest visit-test
  (let [connection (core/connect)
        event (atom nil)
        url "http://www.example.localhost/"]
    (test-helper/with-cache connection {url "<p>foo</p>"})
    (core/visit connection url)
    (is (= (->> (core/select-one connection "p")
                (core/text connection))
           "foo"))))

(deftest select-test
  (let [connection (core/connect)
        url "http://localhost:9222/"]
    (core/visit connection url)
    (let [node-ids (core/select connection "head,body")]
      (is (integer? (first node-ids)))
      (is (= (count node-ids) 2)))))

(deftest select-one-test
  (let [connection (core/connect)
        url "http://localhost:9222/"]
    (core/visit connection url)
    (let [node-id (core/select-one connection "head")]
      (is (integer? node-id)))))

(deftest evaluate-test
  (let [connection (core/connect)
        url "http://www.example.localhost/"]
    (test-helper/with-cache connection {url "<p>foobar<p>"})
    (core/visit connection url)
    (let [some-node-id (core/select-one connection "p")]
      (is (= (core/evaluate connection some-node-id
                            "function() { return 1 + 1; }")
             2))
      (is (= (core/evaluate connection some-node-id
                            "function() { return {x: 42}; }")
             {:x 42}))
      (is (= (core/evaluate connection some-node-id
                            "function() { return this.outerHTML; }")
             "<p>foobar</p>"))
      (is (= (core/evaluate connection some-node-id
                            "function() { return this.innerText; }")
             "foobar")))))

(deftest text-test
  (let [connection (core/connect)
        url "http://www.example.localhost/"
        content "<div><p>foo<b>bar</b></p></div>"]
    (test-helper/with-cache connection {url content})
    (core/visit connection url)
    (let [node-id (core/select-one connection "p")
          text (core/text connection node-id)]
      (is (= text "foobar")))))

(deftest html-test
  (let [connection (core/connect)
        url "http://www.example.localhost/"
        content "<div><p>foo<b>bar</b></p></div>"]
    (test-helper/with-cache connection {url content})
    (core/visit connection url)
    (let [node-id (core/select-one connection "div")
          html (core/html connection node-id)]
      (is (= html content)))))

(deftest click-visit-test
  (let [connection (core/connect)
        main-url "http://www.example.localhost/"
        click-url "http://www.click.localhost/"
        main-content (format "<a href='%s'>click me</a>" click-url)
        click-content "<p>foobar</p>"]
    (test-helper/with-cache connection {main-url main-content
                                        click-url click-content})
    (core/visit connection main-url)
    (->> (core/select-one connection "a")
         (core/click-visit connection))
    (is (= (->> (core/select-one connection "p")
                (core/text connection))
           "foobar"))))

(deftest attributes-test
  (let [connection (core/connect)
        url "http://www.example.localhost/"
        content "<a class='clazz' href='foobar'>click me</a>"]
    (test-helper/with-cache connection {url content})
    (core/visit connection url)
    (is (= (->> (core/select-one connection "a")
                (core/attributes connection))
           {"href" "foobar" "class" "clazz"}))))

(deftest attribute-test
  (let [connection (core/connect)
        url "http://www.example.localhost/"
        content "<a class='clazz' href='foobar'>click me</a>"]
    (test-helper/with-cache connection {url content})
    (core/visit connection url)
    (is (= (->> (core/select-one connection "a")
                (core/attribute connection "href"))
           "foobar"))))
