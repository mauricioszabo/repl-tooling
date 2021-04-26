(ns repl-tooling.editor-integration.definition
  (:require [promesa.core :as p]))

(defn goto-definition [state resolver-opts]
  (let [{:keys [run-callback]} @state
        {:keys [eql]} (:editor/features @state)]
    (-> (eql resolver-opts [:definition/file-name :definition/info :definition/row])
        (p/then (fn [{:definition/keys [info file-name row]}]
                  (if file-name
                    (run-callback :open-editor
                                  (cond-> {:file-name file-name, :line row}

                                          (:file/contents info)
                                          (assoc :contents (:file/contents info))

                                          (:definition/row info)
                                          (assoc :column (:definition/col info))))

                    (run-callback :notify
                                  {:type :error
                                   :title "Could not find definition for var"}))))
        (p/catch #(run-callback :notify
                                {:type :error :title "Could not find definition for var"})))))

(defn goto-current-var [state]
  (goto-definition state nil))
