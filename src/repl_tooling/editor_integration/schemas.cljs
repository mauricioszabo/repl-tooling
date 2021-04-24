(ns repl-tooling.editor-integration.schemas
  (:refer-clojure :exclude [Range])
  (:require [schema.core :as s]))

(def EvalSuccess {:result s/Any :parsed? (s/eq true) :as-text s/Str
                  s/Keyword s/Bool})
(def EvalError {:error s/Any :parsed? (s/eq true) :as-text s/Str s/Keyword s/Bool})
(def ReplResult (s/conditional
                 #(and (contains? % :result) (:parsed? %)) EvalSuccess
                 #(:parsed? %) EvalError
                 #(:literal %) (assoc EvalSuccess
                                      :result s/Str
                                      :literal (s/eq true))
                 #(:interactive %) (assoc EvalSuccess
                                          :result [s/Any]
                                          :interactive (s/eq true))
                 :else EvalSuccess))

(def UnparsedResult (s/conditional
                     #(:parsed? %) ReplResult
                     #(contains? % :error) {:as-text s/Str :error s/Str}
                     :else {:as-text s/Str
                            :result s/Str
                            s/Any s/Any}))

(def Pos [(s/one s/Num 'row) (s/one s/Num 'col)])
(def Range [(s/one Pos 'start) (s/one Pos 'end)])

(def EditorData (s/maybe {:contents s/Str
                          :filename (s/maybe s/Str)
                          :range Range
                          s/Any s/Any}))

(def EvalData {:id s/Symbol
               :editor-data EditorData
               :range Range})

(def EvalResult (assoc EvalData :result ReplResult :repl s/Any))

(def Config {:project-paths [s/Str]
             :eval-mode (s/enum :clj :cljs :prefer-clj :prefer-cljs)
             s/Any s/Any})

(def GotoEditorData
  {:file-name s/Str
   :line s/Int
   (s/optional-key :column) s/Int
   (s/optional-key :contents) s/Str})

(def Command {:name s/Str
              (s/optional-key :description) s/Str
              (s/optional-key :old-command) (s/=> s/Any)
              :command (s/=> s/Any)})

(def Commands {:evaluate-top-block Command
               :evaluate-block Command
               :evaluate-selection Command
               :run-tests-in-ns Command
               :run-test-for-var Command
               :source-for-var Command
               :disconnect Command
               :doc-for-var Command
               :load-file Command
               :go-to-var-definition Command
               (s/optional-key :open-config) Command
               (s/optional-key :break-evaluation) Command
               (s/optional-key :connect-embedded) Command
               s/Keyword Command})

(def Callbacks {:config-file-path (s/maybe s/Str)
                :register-commands (s/=> s/Any Commands)
                :on-start-eval (s/=> s/Any EvalData)
                :open-editor (s/=> s/Any GotoEditorData)
                :on-eval (s/=> s/Any EvalResult)
                :editor-data (s/=> EditorData)
                :notify (s/=> s/Any {:type (s/enum :info :warning :error)
                                     :title s/Str
                                     (s/optional-key :message) s/Str})
                :file-exists (s/=> js/Promise s/Str)
                :read-file (s/=> js/Promise s/Str)
                :get-config (s/=> Config)
                :prompt (s/=> s/Any {:title s/Str
                                     :message s/Str
                                     :arguments [{:key s/Keyword :value s/Str}]})
                :on-copy (s/=> s/Any s/Str)
                :on-stdout (s/=> s/Any s/Str)
                :on-stderr (s/=> s/Any s/Str)
                :on-result (s/=> s/Any ReplResult)
                (s/optional-key :get-rendered-results) (s/=> s/Any)
                (s/optional-key :on-patch) (s/=> s/Any {:id s/Symbol
                                                        :result ReplResult})
                :on-disconnect (s/=> s/Any)})

(def ReplKind (s/enum :clj :cljs :joker :bb :cljr :clje))

(def PossibleRanges (s/enum :top-block :block :var :selection :ns))
(def AuxOptions (s/enum true false :always nil))
(def EvalOpts {(s/optional-key :id) s/Any
               (s/optional-key :namespace) s/Str
               (s/optional-key :range) Range
               (s/optional-key :column) s/Int
               (s/optional-key :pass) {s/Any s/Any}
               (s/optional-key :ignore) s/Bool
               (s/optional-key :aux) AuxOptions
               s/Keyword s/Any})

(def PromisedEvalOpts (assoc EvalOpts
                             :text s/Str
                             (s/optional-key :auto-detect) s/Bool))

(def EditorFeatures {:autocomplete s/Any
                     :eval-and-render (s/=> s/Any s/Any s/Any s/Any)
                     :evaluate-and-render (s/=> s/Any PromisedEvalOpts)
                     :eval (s/=> s/Any PromisedEvalOpts)
                     :result-for-renderer (s/=> js/Promise)
                     :go-to-var-definition (s/=> s/Any {:var-name s/Str
                                                        :namespace s/Str
                                                        :repl s/Any})
                     :get-full-var-name (s/=> js/Promise)
                     :get-code (s/=> js/Promise PossibleRanges)
                     :repl-for (s/=> s/Any s/Str (s/enum true false :always nil))
                     :eql (s/=>* s/Any [s/Any] [{s/Keyword s/Any} s/Any])})

(def EditorState (s/atom {:editor/callbacks Callbacks
                          :editor/features EditorFeatures
                          (s/optional-key :clj/aux) s/Any
                          (s/optional-key :clj/repl) s/Any
                          (s/optional-key :cljs/repl) s/Any
                          (s/optional-key :repl/info)
                          {:host s/Str
                           :port s/Int
                           :kind ReplKind
                           :kind-name s/Str
                           (s/optional-key :cljs/repl-env) s/Any
                           (s/optional-key :cljs/autocomplete-kind) s/Any
                           (s/optional-key :clj/autocomplete-kind) s/Any}
                          :editor/commands Commands
                          :run-callback (s/=> s/Any s/Any s/Any)
                          :run-feature (s/=> s/Any s/Any s/Any)}))

(def OnlyCallbacks (s/atom {:editor/callbacks Callbacks
                            :editor/features (select-keys EditorFeatures [:eql])
                            :editor/commands (select-keys Commands [:doc-for-var
                                                                    :go-to-var-definition])
                            :run-callback (s/=> s/Any s/Any s/Any)
                            :run-feature (s/=> s/Any s/Any s/Any)}))
