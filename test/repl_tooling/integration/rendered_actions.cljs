(ns repl-tooling.integration.rendered-actions
  (:require-macros [repl-tooling.integration.ui-macros :refer [type-and-result]])
  (:require [clojure.core.async :as async :include-macros true]
            [repl-tooling.editor-integration.connection :as conn]
            [repl-tooling.integration.fake-editor :as editor :refer [editor type-and-eval
                                                                     change-stdout]]
            [repl-tooling.integration.ui-macros :as ui :include-macros true]
            [clojure.test :refer [async testing is] :include-macros true]
            [devcards.core :as cards :include-macros true]))

(cards/defcard-rg fake-editor
  editor
  editor/state
  {:inspect-data true})

(cards/deftest copy-to-clipboard
  (async done
    (async/go
     (let [copy (async/promise-chan)]
       (conn/disconnect!)
       (editor/connect! {:on-copy #(async/put! copy %)})
       (async/<! (editor/wait-for #(-> @editor/state :repls :eval)))

       (testing "copies tagged literals to clipboard"
         (type-and-result "(tagged-literal 'foo [1 2])"))

       (conn/disconnect!)
       (done)))))
