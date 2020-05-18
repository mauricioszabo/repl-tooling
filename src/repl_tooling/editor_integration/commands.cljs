(ns repl-tooling.editor-integration.commands
  (:require [repl-tooling.editor-integration.schemas :as schemas]
            [schema.core :as s]))

(defn- run-cmd [cmd state key args]
  (let [callback (get-in @state [key cmd])]
    (when (.-DEBUG js/goog)
      (when (nil? callback) (throw (ex-info "Didn't find function on editor-state"
                                            {:available (-> @state key keys)
                                             :required cmd})))
      (let [schemas (-> schemas/EditorState :schema key cmd :input-schemas first)]
        (s/validate schemas args)))
    (let [res (apply callback args)]
      (when (.-DEBUG js/goog)
        (let [schema (-> schemas/EditorState :schema key cmd :output-schema)]
          (s/validate schema res)))
      res)))

(def CallbackCmd (apply s/enum (keys schemas/Callbacks)))
(s/defn run-callback!
  "Calls a function registered when the REPL was connected"
  [state :- schemas/EditorState, cmd :- CallbackCmd, & args]
  ; (prn :running-callback cmd)
  (run-cmd cmd state :editor/callbacks args))

(def FeatureCmd (apply s/enum (keys schemas/EditorFeatures)))
(s/defn run-feature!
  "Runs an editor command"
  [state :- schemas/EditorState, cmd :- FeatureCmd, & args]
  ; (prn :running-feature cmd)
  (run-cmd cmd state :editor/features args))
