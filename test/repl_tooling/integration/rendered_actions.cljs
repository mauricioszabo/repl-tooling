(ns repl-tooling.integration.rendered-actions
  (:require [clojure.core.async :as async :include-macros true]
            [repl-tooling.editor-integration.connection :as conn]
            [repl-tooling.integration.fake-editor :as editor :refer [editor type-and-eval
                                                                     change-stdout]]
            [repl-tooling.integration.ui-macros :as ui :include-macros true
             :refer [type-and-result]]
            [clojure.test :refer [async testing is] :include-macros true]
            [check.core :refer-macros [check]]
            [devcards.core :as cards :include-macros true]))

(cards/defcard-rg fake-editor
  editor
  editor/state
  {:inspect-data true})

(defn click-clipboard [n]
  (when-let [obj (aget (.. js/document (querySelectorAll "a.icon.clipboard")) n)]
    (.click obj)
    obj))

(defn click-chevron [n]
  (when-let [elem (aget (.. js/document (querySelectorAll "a.chevron")) n)]
    (.click elem)
    elem))

(set! cards/test-timeout 8000)
(cards/deftest copy-to-clipboard
  (async done
    (async/go
     (let [copy (async/chan)]
       (editor/connect! {:on-copy #(async/put! copy %)})
       (async/<! (editor/wait-for #(-> @editor/state :repls :eval)))

       (testing "copies tagged literals to clipboard"
         (type-and-result "(tagged-literal 'foo [1 2])")
         (click-clipboard 0)
         (check (async/<! copy) => "#foo [1 2]"))

       (testing "copy only first line"
         (click-chevron 0)
         (click-clipboard 0)
         (check (async/<! copy) => "#foo [1 2]"))

       (testing "copies colls"
         (editor/wait-for #(click-clipboard 1))
         (check (async/<! copy) => "[1 2]"))

       (testing "copies leafs"
         (click-chevron 1)
         (editor/wait-for #(click-clipboard 2))
         (check (async/<! copy) => "1")
         (click-clipboard 3)
         (check (async/<! copy) => "2"))

       (testing "copies incomplete string"
         (type-and-result "(str (range 80))")
         (click-clipboard 0)
         (check (async/<! copy) => #"28 29"))

       (testing "copies objects"
         (type-and-result "(Object.)")
         (click-clipboard 0)
         (check (async/<! copy) => #"#object.*java\.lang\.Object"))

       (conn/disconnect!)
       (async/close! copy)
       (done)))))
