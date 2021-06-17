(ns repl-tooling.integration.rendered-actions
  (:require [clojure.core.async :as async]
            [repl-tooling.editor-integration.connection :as conn]
            [repl-tooling.integration.fake-editor :as fake]
            [repl-tooling.integration.ui-macros :as ui]
            [clojure.test] ;:refer [async testing is]]
            [check.async :refer [testing async-test check]]
            [promesa.core :as p]
            [devcards.core :as cards]))

(cards/defcard-rg fake-editor
  fake/editor
  fake/state
  {:inspect-data true})

(defn click-chevron [n]
  (fake/wait-for-p
   #(when-let [elem (aget (.. js/document (querySelectorAll "a.chevron")) n)]
      (.click elem)
      elem)))

(def ^:private clipboard (atom nil))

(defn click-clipboard [n]
  (reset! clipboard (p/deferred))
  (fake/wait-for-p
   #(when-let [obj (aget (js/document.querySelectorAll "a.icon.clipboard")
                         n)]
      (.click obj)
      obj)))

(set! cards/test-timeout 20000)
(cards/deftest copy-to-clipboard
  (let [copy (async/chan)]
    (async-test "actions that can be made after rendering a result"
      {:teardown (fake/disconnect!)}

      (fake/connect! {:on-copy #(do
                                  (prn :COPY %)
                                  (p/resolve! @clipboard %))})

      (testing "copies tagged literals to clipboard"
        (fake/type-and-eval "(tagged-literal 'foo [1 2])")
        (fake/change-result-p)
        (click-clipboard 0)
        (check @clipboard => "#foo [1 2]"))

      (testing "copy only first line"
        (click-chevron 0)
        (click-clipboard 0)
        (check @clipboard => "#foo [1 2]"))

      (testing "copies colls"
        (click-clipboard 1)
        (check @clipboard => "[1 2]"))

      (testing "copies leafs"
        (click-chevron 1)
        (click-clipboard 2)
        (check @clipboard => "1")
        (click-clipboard 3)
        (check @clipboard => "2"))

      (testing "copies incomplete string"
        (fake/type-and-eval "(str (range 80))")
        (fake/change-result-p)
        (click-clipboard 0)
        (check @clipboard => #"28 29"))

      (testing "copies objects"
        (fake/type-and-eval "(Object.)")
        (fake/change-result-p)
        (click-clipboard 0)
        (check @clipboard => #"#object.*java\.lang\.Object")))))
