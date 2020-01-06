(ns repl-tooling.editor-integration.schemas
  (:refer-clojure :exclude [Range])
  (:require [schema.core :as s]))

(def EvalSuccess {:result s/Any :parsed? (s/eq true) :as-text s/Str
                  s/Keyword s/Bool})
(def EvalError {:error s/Any :parsed? (s/eq true) :as-text s/Str})
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

(def EvalResult (assoc EvalData :result ReplResult))

(def Config {:project-paths [s/Str]
             :eval-mode (s/enum :clj :cljs :prefer-clj :prefer-cljs)
             s/Any s/Any})

(def Callbacks {:on-start-eval (s/=> s/Any EvalData)
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
                :on-disconnect (s/=> s/Any)})
