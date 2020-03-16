(ns repl-tooling.editor-integration.definition
  (:require [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.features.definition :as def]
            [repl-tooling.editor-helpers :as helpers]))

(defn goto-var [editor-data state]
  (let [{:keys [contents range editor filename]} editor-data
        callbacks (:editor/callbacks @state)
        {:keys [open-editor notify]} callbacks
        [_ var] (helpers/current-var contents (first range))
        [_ namespace] (helpers/ns-range-for contents (first range))
        aux (:clj/aux @state)
        repl (e-eval/repl-for callbacks state filename true)]
    (when-not
      (some-> repl (def/find-var-definition aux namespace var)
              (.then #(open-editor %)))
      (notify {:type :error :title "Could not find definition for var"}))))
