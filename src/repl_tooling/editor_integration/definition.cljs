(ns repl-tooling.editor-integration.definition
  (:require [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.features.definition :as def]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.editor-integration.commands :as cmds]
            [promesa.core :as p]
            [clojure.string :as str]
            ["os" :refer [platform]]))

(defn goto-current-var [state]
  (let [{:keys [run-callback]} @state
        {:keys [eql]} (:editor/features @state)]
    (-> (eql [:definition/info])
        (p/then #(run-callback :open-editor (:definition/info %)))
        (p/catch #(run-callback :notify
                                {:type :error :title "Could not find definition for var"})))))
