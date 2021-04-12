(ns repl-tooling.editor-integration.definition
  (:require [promesa.core :as p]))

(defn goto-definition [state resolver-opts]
  (let [{:keys [run-callback]} @state
        {:keys [eql]} (:editor/features @state)]
    (-> (eql resolver-opts [:definition/info :definition/line])
        (p/then (fn [{:definition/keys [info line]}]
                  (if info
                    (run-callback :open-editor (assoc info :line line))
                    (run-callback :notify
                                  {:type :error
                                   :title "Could not find definition for var"}))))
        (p/catch #(run-callback :notify
                                {:type :error :title "Could not find definition for var"})))))

(defn goto-current-var [state]
  (goto-definition state nil))
