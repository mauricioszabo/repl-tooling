(ns repl-tooling.editor-integration.doc-test
  (:require [clojure.string :as str]
            [repl-tooling.integration.fake-editor :as fake]
            [repl-tooling.editor-integration.doc :as doc]
            [repl-tooling.editor-integration.renderer :as render]
            [repl-tooling.eval :as repl]
            [clojure.test :refer [testing]]
            [check.core :refer [check]]
            [check.async :refer [async-test await!]]
            [clojure.core.async :as async]
            [repl-tooling.eval-helpers :as h]
            [repl-tooling.integration.ui-macros :as m]
            [devcards.core :as cards]))
            ; [repl-tooling.commands-to-repl.doc-and-spec :as sdoc]))

(cards/deftest orchard-info
  (async-test "with editor infrastructure" {:timeout 8000
                                            :teardown (fake/disconnect!)}
    (await! (fake/connect!))

    (testing "Info for Clojure vars"
      (fake/type "str")
      (fake/run-command! :info-for-var)
      (check (await! (fake/change-result)) => #"With no args,")

      (m/click-on "clojure.core/prn")
      (check (await! (fake/change-result)) => #"Same as pr followed"))

    (testing "Info for Java methods"
      (fake/type ".toUpperCase")
      (fake/run-command! :info-for-var)
      (check (await! (fake/change-result)) => #"toUpperCase"))))

(cards/defcard-rg fake-editor
  fake/editor
  fake/state)
