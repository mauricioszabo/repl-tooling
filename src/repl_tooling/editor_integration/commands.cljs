(ns repl-tooling.editor-integration.commands
  (:require [repl-tooling.editor-integration.schemas :as schemas]
            [schema.core :as s]))

(defn- run-cmd [cmd state key args]
  (let [callback (get-in @state [key cmd])]
    (when (.-DEBUG js/goog)
      (let [schemas (-> schemas/EditorState :schema key cmd :input-schemas first)]
        (s/validate schemas args)))
    (let [res (apply callback args)]
      (when (.-DEBUG js/goog)
        (let [schema (-> schemas/EditorState :schema key cmd :output-schema)]
          (s/validate schema res))))))

(def CallbackCmd (apply s/enum (keys schemas/Callbacks)))
(s/defn callback!
  "Calls a function registered when the REPL was connected"
  [state :- schemas/EditorState, cmd :- CallbackCmd, & args]
  (run-cmd cmd state :editor/callbacks args))
