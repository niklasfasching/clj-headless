(ns headless.generate-test
  (:require [clojure.test :refer :all]
            [headless.core :as core]
            [headless.generate :as generate]))

(deftest lisp-symbol-test
  (is (= (generate/lisp-symbol "CSS.addRule")
         'css-add-rule))
  (is (= (generate/lisp-symbol "DOMDebugger.setDOMBreakpoint")
         'dom-debugger-set-dom-breakpoint))
  (is (= (generate/lisp-symbol "DOM.getOuterHTML")
         'dom-get-outer-html)))
