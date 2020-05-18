(ns repl-tooling.editor-integration.definition
  (:require [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.features.definition :as def]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.editor-integration.commands :as cmds]))

(defn goto-var [{:keys [var-name namespace repl state]}]
  (when-not
    (some-> repl (def/find-var-definition (:clj/aux @state) namespace var-name)
            (.then #(cmds/run-callback! state :open-editor %)))
    (cmds/run-callback! state
                        :notify
                        {:type :error :title "Could not find definition for var"})))

(defn goto-current-var [editor-data state]
  (let [{:keys [contents range editor filename]} editor-data
        [_ var] (helpers/current-var contents (first range))
        [_ namespace] (helpers/ns-range-for contents (first range))
        repl (e-eval/repl-for state filename true)]
    (goto-var {:var-name var :namespace namespace :repl repl :state state})))
