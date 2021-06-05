(ns repl-tooling.editor-integration.definition
  (:require [promesa.core :as p]))

(defn goto-definition [state resolver-opts]
  (let [{:keys [run-callback]} @state
        {:keys [eql]} (:editor/features @state)]
    (-> (eql resolver-opts [:definition/filename
                            :definition/contents
                            :definition/row
                            :definition/col])
        (p/then (fn [{:definition/keys [contents col filename row]}]
                  (if filename
                    (run-callback :open-editor
                                  (cond-> {:file-name filename, :line row}

                                          contents
                                          (assoc :contents (:text/contents contents))

                                          col
                                          (assoc :column col)))

                    (run-callback :notify
                                  {:type :error
                                   :title "Could not find definition for var"}))))
        (p/catch #(run-callback :notify
                                {:type :error :title "Could not find definition for var"})))))

(defn goto-current-var [state]
  (goto-definition state nil))
