(ns repl-tooling.features.definition-test
  (:require [clojure.test :refer-macros [testing async is]]
            [devcards.core :as cards :include-macros true]
            [check.core :refer-macros [check]]
            [check.async :refer-macros [await!]]
            [clojure.core.async :as async :include-macros true]
            [repl-tooling.repl-client :as client]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.repl-client.clojure :as clj]
            [repl-tooling.features.definition :as def]
            [repl-tooling.eval-helpers :refer-macros [eval-on-repl async-with-repl]]))

(set! cards/test-timeout 8000)
(cards/deftest finding-definition
  (async-with-repl "finding definition on Clojure"
    (eval-on-repl "(require '[repl-tooling.features.definition-helper :reload :all])")

    (testing "finds symbols inside jars, and get file's contents"
      (check (await! (def/find-var-definition repl 'user "prn"))
             => {:line number? :file-name string? :contents string?}))

    (testing "finds symbols inside other namespaces, and gets file"
      (check (await! (def/find-var-definition repl
                       'repl-tooling.features.definition-helper "c/some-function"))
             => {:line number?
                 :file-name #"repl_tooling/features/definition_child\.clj"})

      (check (await! (def/find-var-definition repl
                       'repl-tooling.features.definition-helper "other-var"))
             => {:file-name #"repl_tooling/features/definition_child\.clj"}))

    (testing "finds symbols inside same namespace, and gets file"
      (check (await! (def/find-var-definition repl
                       'repl-tooling.features.definition-helper "some-function"))
             => {:line 3 :file-name #"repl_tooling/features/definition_helper\.clj"}))

    (testing "load file, then find symbol"
      (eval-on-repl "(load-file \"test/repl_tooling/features/definition_helper.clj\")")
      (check (await! (def/find-var-definition repl
                       'repl-tooling.features.definition-helper "some-function"))
             => {:line 3 :file-name #"repl_tooling/features/definition_helper\.clj"}))))
