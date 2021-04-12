(ns repl-tooling.editor-integration.definition
  (:require [promesa.core :as p]))

(defn goto-current-var [state]
  (let [{:keys [run-callback]} @state
        {:keys [eql]} (:editor/features @state)]
    (-> (eql [:definition/info])
        (p/then #(run-callback :open-editor (:definition/info %)))
        (p/catch #(run-callback :notify
                                {:type :error :title "Could not find definition for var"})))))
