(ns repl-tooling.commands-to-repl.all-cmds
  (:require [promesa.core :as p]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.commands-to-repl.orchard :as orchard]
            [repl-tooling.eval :as eval]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.editor-integration.loaders :as loaders]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.editor-integration.embedded-clojurescript :as embedded]
            [repl-tooling.editor-integration.definition :as definition]
            [repl-tooling.editor-integration.doc :as doc]
            [repl-tooling.editor-integration.commands :as cmds]))

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
        [eval-range code] (function contents start)]
    (when eval-range
      (let [[_ namespace] (helpers/ns-range-for contents (first eval-range))]
        (e-eval/eval-cmd state code namespace eval-range data opts)))))

(defn- eval-block [state data]
  (p/let [d data]
    (eval-range state d {} helpers/block-for)))

(defn- eval-top-block [state data]
  (p/let [d data]
    (eval-range state d {} helpers/top-block-for)))

(defn- eval-selection [state data]
  (p/let [{:keys [range] :as d} data]
    (eval-range state d {} (fn [contents _]
                             [range (helpers/text-in-range contents range)]))))

(defn all [state {:keys [editor-data] :as opts} repl-kind]
  (p/let [orchard-cmds (orchard/cmds state)
          config-file (-> @state :editor/callbacks :config-file-path)]
    (cond->
     {:evaluate-top-block {:name "Evaluate Top Block"
                           :description "Evaluates top block block on current editor's selection"
                           :command #(eval-top-block state (editor-data))}
      :evaluate-block {:name "Evaluate Block"
                       :description "Evaluates current block on editor's selection"
                       :command #(eval-block state (editor-data))}
      :evaluate-selection {:name "Evaluate Selection"
                           :description "Evaluates current editor's selection"
                           :command #(eval-selection state (editor-data))}
      :run-tests-in-ns {:name "Run tests in NS"
                        :description "Run all tests on the current namespace"
                        :command #(e-eval/run-tests-in-ns! state)}
      :run-test-for-var {:name "Run test for current Var"
                         :description "Run current var as a testcase"
                         :command #(p/let [data (editor-data)]
                                     (e-eval/run-test-at-cursor! state data))}
      :source-for-var {:name "Source for Var"
                       :description "Gets the source of the current var"
                       :command #(e-eval/source-for-var! state)}
      :disconnect {:name "Disconnect REPLs"
                   :description "Disconnect all current connected REPLs"
                   :command disconnect!}
      :doc-for-var {:name "Documentation for current var"
                    :description "Shows documentation for the current var under cursor"
                    :command #(doc/doc-for-var state)}
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
                             :command #(definition/goto-current-var state)}}

     config-file
     (assoc :open-config {:name "Open Config File"
                          :description "Opens the current config file"
                          :command #(cmds/run-callback! state :open-editor
                                                        {:file-name config-file
                                                          :line 0})})

     (= :clj repl-kind)
     (assoc
      :break-evaluation {:name "Break Evaluation"
                         :description "Break current running eval"
                         :command #(eval/break (:clj/repl @state) (:clj/aux @state))}
      :connect-embedded {:name "Connect Embedded ClojureScript REPL"
                         :description "Connects to a ClojureScript REPL inside a Clojure one"
                         :command #(embedded/connect! state opts true)
                         :old-command #(embedded/connect! state opts false)})

     :always (merge orchard-cmds))))

(defn fqn-for-var [editor-state]
  (p/let [{:keys [contents range filename]} (cmds/run-callback! editor-state :editor-data)
          [range var] (helpers/current-var contents (first range))
          res (cmds/run-feature! editor-state :eval
                                 {:text (str "`" var)
                                  :ignore true :auto-detect true :aux true})]
    (assoc res :range range)))

(defn static-commands [state-ish]
  (let [config-file (-> @state-ish :editor/callbacks :config-file-path)]
    (cond->
     {:doc-for-var {:name "Documentation for current var"
                    :description "Shows documentation for the current var under cursor"
                    :command #(doc/doc-for-var state-ish)}
      :go-to-var-definition {:name "Goto VAR definition"
                             :description "Goes to definition of the current variable"
                             :command #(definition/goto-current-var state-ish)}})))

     ; config-file
     ; (assoc :open-config {:name "Open Config File"
     ;                      :description "Opens the current config file"
     ;                      :command #(cmds/run-callback! state :open-editor
     ;                                                    {:file-name config-file})}))))
