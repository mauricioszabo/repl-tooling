(ns repl-tooling.commands-to-repl.all-cmds
  (:require [promesa.core :as p]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.editor-integration.loaders :as loaders]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.editor-integration.embedded-clojurescript :as embedded]
            [repl-tooling.editor-integration.definition :as definition]
            [repl-tooling.editor-integration.doc :as doc]))

(defn disconnect!
  "Disconnect all REPLs. Indempotent."
  []
  (repls/disconnect! :clj-eval)
  (repls/disconnect! :clj-aux)
  (repls/disconnect! :cljs-aux)
  (repls/disconnect! :cljs-eval))

(defn handle-disconnect!
  "Disconnect all REPLs. Indempotent."
  [state]
  (disconnect!)
  (reset! state nil))

(defn eval-range [state {:keys [contents range] :as data} opts function]
  (let [[start] range
        [eval-range code] (function contents start)
        [_ namespace] (helpers/ns-range-for contents (first eval-range))]
    (e-eval/eval-cmd state code namespace eval-range data opts)))

(defn- eval-block [state data opts]
  (p/let [d data]
    (eval-range state d opts helpers/block-for)))

(defn- eval-top-block [state data opts]
  (p/let [d data]
    (eval-range state d opts helpers/top-block-for)))

(defn- eval-selection [state data opts]
  (p/let [{:keys [range] :as d} data]
    (eval-range state d opts (fn [contents _]
                               [range (helpers/text-in-range contents range)]))))

(defn all [state {:keys [editor-data] :as opts} repl-kind]
  (cond->
   {:evaluate-top-block {:name "Evaluate Top Block"
                         :description "Evaluates top block block on current editor's selection"
                         :command #(eval-top-block state (editor-data) opts)}
    :evaluate-block {:name "Evaluate Block"
                     :description "Evaluates current block on editor's selection"
                     :command #(eval-block state (editor-data) opts)}
    :evaluate-selection {:name "Evaluate Selection"
                         :description "Evaluates current editor's selection"
                         :command #(eval-selection state (editor-data) opts)}
    :run-tests-in-ns {:name "Run tests in NS"
                      :description "Run all tests on the current namespace"
                      :command #(e-eval/run-tests-in-ns! state)}
    :run-test-for-var {:name "Run test for current Var"
                       :description "Run current var as a testcase"
                       :command #(p/let [data (editor-data)]
                                   (e-eval/run-test-at-cursor! state data))}
    :source-for-var {:name "Source for Var"
                     :description "Gets the source of the current var"
                     :command #(p/let [data (editor-data)]
                                 (e-eval/source-for-var! state data))}
    :disconnect {:name "Disconnect REPLs"
                 :description "Disconnect all current connected REPLs"
                 :command #(handle-disconnect! state)}
    :doc-for-var {:name "Documentation for current var"
                  :description "Shows documentation for the current var under cursor"
                  :command #(p/let [data (editor-data)]
                              (doc/doc-for-var data opts state))}
    ; :spec-for-var {:name "Spec for current var"
    ;                :description "Shows spec for the current var under cursor if it exists"
    ;                :command (fn [] (ensure-data (editor-data)
    ;                                             #(doc/specs-for-var % opts state)))}
    :load-file {:name "Load File"
                :description "Loads current file on a Clojure REPL"
                :command #(p/let [data (editor-data)]
                            (loaders/load-file data @state))}
    :go-to-var-definition {:name "Goto VAR definition"
                           :description "Goes to definition of the current variable"
                           :command #(p/let [data (editor-data)]
                                       (definition/goto-var data state))}}

   (= :clj repl-kind)
   (assoc
    :break-evaluation {:name "Break Evaluation"
                       :description "Break current running eval"
                       :command #(eval/break (:clj/repl @state) (:clj/aux @state))}
    :connect-embedded {:name "Connect Embedded ClojureScript REPL"
                       :description "Connects to a ClojureScript REPL inside a Clojure one"
                       :command #(embedded/connect! state opts true)
                       :old-command #(embedded/connect! state opts false)})))
