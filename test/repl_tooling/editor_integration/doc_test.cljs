(ns repl-tooling.editor-integration.doc-test
  (:require [clojure.string :as str]
            [repl-tooling.integration.fake-editor :as fake]
            [repl-tooling.editor-integration.doc :as doc]
            [repl-tooling.editor-integration.renderer :as render]
            [repl-tooling.eval :as repl]
            [clojure.test]
            [promesa.core :as p]
            [check.async :refer [async-test testing check]]
            [clojure.core.async :as async]
            [repl-tooling.integration.ui-macros :as m]
            [devcards.core :as cards]))

;; FIXME: some problems with this test...
#_
(cards/deftest orchard-info
  (async-test "with editor infrastructure" {:timeout 8000
                                            :teardown (fake/disconnect!)}
    (fake/connect!)

    (testing "Info for Clojure vars"
      (fake/type "str")
      (fake/run-command! :info-for-var)
      (check (fake/change-result-p) => #"With no args,")

      (m/click-on "clojure.core/prn")
      (check (fake/change-result-p) => #"Same as pr followed"))

    (testing "Info for Java methods"
      (fake/type ".toUpperCase")
      (fake/run-command! :info-for-var)
      (check (fake/change-result-p) => #"toUpperCase"))))

(cards/defcard-rg fake-editor
  fake/editor
  fake/state)
