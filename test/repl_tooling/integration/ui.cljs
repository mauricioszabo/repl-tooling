(ns repl-tooling.integration.ui
  (:require [devcards.core :as cards :include-macros true]
            [schema.core :as s]

            [repl-tooling.editor-helpers-test]
            [repl-tooling.repl-client.parsing-test]
            [repl-tooling.repl-client.textual-representation-test]
            [repl-tooling.integration.clojurescript-ui]
            [repl-tooling.integration.clojure-ui]
            [repl-tooling.repl-client.evaluation-test]
            [repl-tooling.features.autocomplete-test]
            [repl-tooling.features.shadow-cljs-test]
            [repl-tooling.editor-integration.autocomplete-test]
            [repl-tooling.repl-client.connection-test]
            [repl-tooling.integration.rendered-actions]
            [repl-tooling.editor-integration.renderer.interactive-test]
            [repl-tooling.editor-integration.doc-test]
            [repl-tooling.nrepl.nrepl-test]
            [repl-tooling.editor-integration.configs-test]
            [repl-tooling.editor-integration.pathom-test]
            [repl-tooling.editor-integration.stacktraces-test]))

(cards/start-devcard-ui!)
(s/set-fn-validation! true)
