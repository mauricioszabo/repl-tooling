(ns repl-tooling.features.definition-test
  (:require [clojure.test]
            [devcards.core :as cards]
            [check.async :refer [check async-test testing]]
            [promesa.core :as p]
            ; [check.async-old :refer [await!]]
            ; [clojure.core.async :as async]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.features.definition :as def]
            [repl-tooling.integration.fake-editor :as fake]
            [repl-tooling.eval-helpers
             :refer [eval-on-repl async-with-repl async-with-cljs-repl]]))

(def config (atom {:eval-mode :prefer-clj
                   :project-paths [(. js/process cwd)]}))

(set! cards/test-timeout 20000)
(cards/deftest finding-definition
  (async-test "finding definition on Clojure" {:teardown (fake/disconnect!)
                                               :timeout 8000}
    (fake/connect! {:get-config #(deref config)})
    (fake/run-command! :connect-embedded)

    (fake/run-feature! :eval
                       {:text "(require '[repl-tooling.features.definition-helper :reload :all])"})

    (testing "getting var definition from core locations"
      (swap! fake/state assoc :range [[2 1] [2 1]] :code "(ns user)\n\n(prn 1 2)")
      (check (fake/run-feature! :eql
                                {:editor-state (:editor-state @fake/state)}
                                [:definition/file-name :definition/line :definition/info])
             => {:definition/file-name #"clojure.*jar!/clojure/core.clj"
                 :definition/line number?
                 :definition/info {:file/contents string?}}))

    (testing "finds definition of namespace"
      (swap! fake/state assoc :range [[0 4] [0 4]]
             :code "(ns repl-tooling.features.definition-helper)")
      (check (fake/run-feature! :eql [:definition/file-name :definition/line])
             => {:definition/line 0
                 :definition/file-name #"repl_tooling/features/definition_helper\.clj"}))

    (testing "finds symbols inside jars, and get file's contents"
      (swap! fake/state assoc :range [[2 1] [2 1]] :code "(ns user)\n\n(prn 1 2)")
      (check (fake/run-feature! :eql [:definition/info
                                      :definition/file-name
                                      :definition/line])
             => {:definition/line number?
                 :definition/file-name string?
                 :definition/info {:file/contents string?}}))

    (testing "finds symbols inside other namespaces, and gets file"
      (swap! fake/state assoc :range [[2 1] [2 1]]
             :code "(ns repl-tooling.features.definition-helper)\n\nc/some-function")
      (check (fake/run-feature! :eql [:definition/file-name :definition/line])
             => {:definition/line number?
                 :definition/file-name #"repl_tooling/features/definition_child\.clj"})

      (fake/type "(ns repl-tooling.features.definition-helper)\n\nother-var")
      (check (fake/run-feature! :eql [:definition/file-name :definition/line])
             => {:definition/line 7
                 :definition/file-name #"repl_tooling/features/definition_child\.clj"}))

    (testing "finds symbols inside same namespace, and gets file"
      (fake/type "(ns repl-tooling.features.definition-helper)\n\nsome-function")
      (check (fake/run-feature! :eql [:definition/file-name :definition/line])
             => {:definition/line 3
                 :definition/file-name #"repl_tooling/features/definition_helper\.clj"}))

    (swap! config assoc :eval-mode :cljs)
    (testing "getting definition on current NS for ClojureScript"
      (fake/type "(ns repl-tooling.integration.fixture-app)\n\nlocal-fn")
      (check (fake/run-feature! :eql [:definition/file-name :definition/line])
             => {:definition/line 9
                 :definition/file-name #"test/repl_tooling/integration/fixture_app\.cljs"}))

    (testing "getting path of stacktrace"
      (fake/type "(ns repl-tooling.integration.fixture-app)\n\nlocal-fn")
      (check (fake/run-feature! :eql
                                {:ex/function-name "clojure.core/fn/eval1234"
                                 :ex/filename "core.clj"
                                 :ex/line 9}
                                [:definition/file-name :definition/line])
             => {:definition/line 8
                 :definition/file-name #"clojure.*jar!/clojure/core.clj"}))))
