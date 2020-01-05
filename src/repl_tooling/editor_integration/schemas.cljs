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

; (def Callbacks {:on-start-eval
;                 :on-eval
;                 :editor-data
;                 :notify
;                 :get-config
;                 :prompt
;                 :on-copy
;                 :on-stdout
;                 :on-stdout
;                 :on-stderr
;                 :on-result
;                 :on-disconnect})
  
  ; (s/def ::contents string?)
  ; (s/def ::filename string?)
  ; (s/def ::row-col (s/tuple int? int?))
  ; (s/def ::range (s/tuple ::row-col ::row-col))
  ; (s/def :ret/editor-data (s/keys :req-un [::contents ::filename ::range]))
  ;
  ; (s/def ::id symbol?)
  ; (s/def ::eval-data (s/keys :req-un [::id :ret/editor-data ::range]))
  ;
  ; ; REPL
  ; (s/def :eval/result any?)
  ; (s/def :eval.string/result string?)
  ; (s/def :eval.vector/result vector?)
  ; (s/def :eval/error any?)
  ;
  ; (s/def :repl/parsed? true?)
  ; (s/def :repl/literal true?)
  ; (s/def :repl/interactive true?)
  ; (s/def :repl/result (s/or :res (s/keys :req-un [:eval/result :repl/parsed?])
  ;                           :literal (s/and (s/keys :req-un [:eval.string/result
  ;                                                            :repl/literal
  ;                                                            :repl/parsed?]))
  ;                           :interactive (s/and (s/keys :req-un [:eval.vector/result
  ;                                                                :repl/interactive
  ;                                                                :repl/parsed?]))))
  ; (s/def :repl/error (s/keys :req-un [:eval/error :repl/parsed?]))
  ;
  ; (s/def ::result (s/or :result :repl/result
  ;                       :error :repl/error))
  ; (s/def ::eval-result (s/keys :req-un [::id :ret/editor-data ::range ::result]))
  ;
  ; (s/def ::on-start-eval (s/fspec :args (s/cat :eval-data ::eval-data)))
  ; (s/def ::on-eval (s/fspec :args (s/cat :eval-data ::eval-result)))
  ; (s/def ::editor-data (s/fspec :ret :ret/editor-data))
  ;
  ; (s/def ::type #{:info :warning :error})
  ; (s/def ::title string?)
  ; (s/def ::message string?)
  ; (s/def ::notify (s/fspec :args (s/keys :req-un [::type ::title ::message])))
  ;
  ; (s/def ::project-paths (s/coll-of string?))
  ; (s/def ::eval-mode #{:clj :cljs :prefer-clj :prefer-cljs})
  ; (s/def ::configs (s/keys :req-un [::project-paths ::eval-mode]))
  ; (s/def ::get-config (s/fspec :ret ::configs))
  ;
  ; (s/def ::prompt (s/fspec :ret #(instance? js/Promise %)))
  ;
  ; (s/def ::on-copy (s/fspec :args (s/cat :txt string?)))
  ; (s/def ::on-stdout (s/fspec :args (s/cat :txt string?)))
  ; (s/def ::on-stderr (s/fspec :args (s/cat :txt string?)))
  ; (s/def ::on-result (s/fspec :args (s/cat :parsed-result any?)))
  ; (s/def ::on-disconnect fn?)
  ;
  ; (s/def ::callbacks (s/keys :req-un [::on-start-eval
  ;                                     ::on-eval
  ;                                     ::editor-data
  ;                                     ::notify
  ;                                     ::get-config
  ;                                     ::prompt
  ;                                     ::on-copy
  ;                                     ::on-stdout
  ;                                     ::on-stdout
  ;                                     ::on-stderr
  ;                                     ::on-result
  ;                                     ::on-disconnect]))
