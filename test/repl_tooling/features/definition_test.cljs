(ns repl-tooling.features.definition-test
  (:require [clojure.test :refer [testing async is]]
            [devcards.core :as cards]
            [check.core :refer [check]]
            [check.async :refer [await!]]
            [clojure.core.async :as async]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.features.definition :as def]
            [repl-tooling.eval-helpers
             :refer [eval-on-repl async-with-repl async-with-cljs-repl]]))

(set! cards/test-timeout 20000)
(cards/deftest finding-definition
  (async-with-repl "finding definition on Clojure"
    (eval-on-repl "(require '[repl-tooling.features.definition-helper :reload :all])")

    (testing "finds symbols inside jars, and get file's contents"
      (check (await! (def/find-var-definition repl repl 'user "prn"))
             => {:line number? :file-name string? :contents string?}))

    (testing "finds symbols inside other namespaces, and gets file"
      (check (await! (def/find-var-definition repl repl
                       'repl-tooling.features.definition-helper "c/some-function"))
             => {:line number?
                 :file-name #"repl_tooling/features/definition_child\.clj"})

      (check (await! (def/find-var-definition repl repl
                       'repl-tooling.features.definition-helper "other-var"))
             => {:file-name #"repl_tooling/features/definition_child\.clj"}))

    (testing "finds symbols inside same namespace, and gets file"
      (check (await! (def/find-var-definition repl repl
                       'repl-tooling.features.definition-helper "some-function"))
             => {:line 3 :file-name #"repl_tooling/features/definition_helper\.clj"}))

    (testing "load file, then find symbol"
      (eval-on-repl "(load-file \"test/repl_tooling/features/definition_helper.clj\")")
      (check (await! (def/find-var-definition repl repl
                       'repl-tooling.features.definition-helper "some-function"))
             => {:line 3 :file-name #"repl_tooling/features/definition_helper\.clj"}))))

(cards/deftest finding-definition-in-cljs
  (async-with-cljs-repl "finding definition on ClojureScript"
    (testing "getting definition on current NS"
      (check (await! (def/find-var-definition repl aux
                       'repl-tooling.integration.fixture-app "local-fn"))
             => {:line 7
                 :file-name #"test/repl_tooling/integration/fixture_app\.cljs"}))))
