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

(def EditorData {:contents s/Str
                 :filename s/Str
                 :range Range
                 s/Any s/Any})

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

(def Callbacks {:on-start-eval (s/=> s/Any EvalData)
                :open-editor (s/=> s/Any GotoEditorData)
                :on-eval (s/=> s/Any EvalResult)
                :editor-data (s/=> EditorData)
                :notify (s/=> s/Any {:type (s/enum :info :warning :error)
                                     :title s/Str
                                     (s/optional-key :message) s/Str})
                :get-config (s/=> Config)
                :prompt (s/=> s/Any {:title s/Str
                                     :message s/Str
                                     :arguments [{:key s/Symbol :value s/Str}]})
                :on-copy (s/=> s/Any s/Str)
                :on-stdout (s/=> s/Any s/Str)
                :on-stderr (s/=> s/Any s/Str)
                :on-result (s/=> s/Any ReplResult)
                (s/optional-key :get-rendered-results) (s/=> s/Any)
                (s/optional-key :on-patch) (s/=> s/Any {:id s/Symbol
                                                        :result ReplResult})
                :on-disconnect (s/=> s/Any)})

(def Commands {:evaluate-top-block s/Any
               :evaluate-block s/Any
               :evaluate-selection s/Any
               :run-tests-in-ns s/Any
               :run-test-for-var s/Any
               :source-for-var s/Any
               :disconnect s/Any
               :doc-for-var s/Any
               :load-file s/Any
               :go-to-var-definition s/Any
               (s/optional-key :break-evaluation) s/Any
               (s/optional-key :connect-embedded) s/Any})

(def ReplKind (s/enum :clj :cljs :joker :bb :clr))

(def EditorFeatures {:autocomplete s/Any
                     :eval-and-render s/Any
                     :eval (s/=> s/Any s/Any s/Any)
                     :result-for-renderer js/Promise
                     :go-to-var-definition (s/=> s/Any {:var-name s/Str
                                                        :namespace s/Str
                                                        :repl s/Any})})

(def EditorState (s/atom {:editor/callbacks Callbacks
                               :editor/features EditorFeatures
                               (s/optional-key :clj/aux) s/Any
                               (s/optional-key :clj/repl) s/Any
                               (s/optional-key :cljs/repl) s/Any
                               (s/optional-key :repl/info) {:host s/Str
                                                            :port s/Int
                                                            :kind ReplKind
                                                            :kind-name s/Str}
                               :editor/commands Commands}))
