(ns repl-tooling.editor-integration.commands
  (:require [repl-tooling.editor-integration.schemas :as schemas]
            [schema.core :as s]
            [reagent.ratom :as ratom]))

(defn- run-cmd [cmd state key args]
  (let [callback (get-in @state [key cmd])]
    (when (.-DEBUG js/goog)
      (when (nil? callback) (throw (ex-info "Didn't find function on editor-state"
                                            {:available (-> @state key keys)
                                             :required cmd})))
      (let [schemas (->> schemas/EditorState :schema key cmd :input-schemas
                         (filter #(-> % count (= (count args))))
                         first)]
        (s/validate schemas args)))
    (let [res (apply callback args)]
      (when (.-DEBUG js/goog)
        (let [schema (-> schemas/EditorState :schema key cmd :output-schema)]
          (s/validate schema res)))
      res)))

(def CallbackCmd (apply s/enum (keys schemas/Callbacks)))
(s/defn state-run-callback!
  "Calls a function registered when the REPL was connected"
  [state :- schemas/EditorState, cmd :- CallbackCmd, args]
  (run-cmd cmd state :editor/callbacks args))

(def FeatureCmd (apply s/enum (keys schemas/EditorFeatures)))
(s/defn state-run-feature!
  "Runs an editor command"
  [state :- schemas/EditorState, cmd :- FeatureCmd, args]
  (run-cmd cmd state :editor/features args))

(defprotocol CommandRunner
  (-run-callback! [this callback args])
  (-run-feature! [this feature args]))

(extend-protocol CommandRunner
  ratom/RAtom
  (-run-callback! [this callback args]
    (state-run-callback! this callback args))
  (-run-feature! [this feature args]
    (state-run-feature! this feature args))

  Atom
  (-run-callback! [this callback args]
    (state-run-callback! this callback args))
  (-run-feature! [this feature args]
    (state-run-feature! this feature args)))

(defrecord Callbacks [quasi-state]
  CommandRunner
  (-run-callback! [this callback args]
    (run-cmd callback quasi-state :editor/callbacks args))
  (-run-feature! [this feature args]
    (run-cmd feature quasi-state :editor/features args)))

(defn run-callback! [this callback & args]
  (when this (-run-callback! this callback args)))
(defn run-feature! [this feature & args]
  (when this (-run-feature! this feature args)))
