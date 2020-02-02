(clojure.core/let [res-fn
                   (clojure.core/fn res-fn [res change-keywords?]
                     (clojure.core/cond
                       (clojure.core/instance? cljs.core/ExceptionInfo res)
                       (clojure.core/tagged-literal 'error
                         {:type "cljs.core.ExceptionInfo"
                          :data (.-data res)
                          :message (.-message res)
                          :trace (clojure.core/->> res .-stack clojure.string/split-lines)})

                       (clojure.core/instance? js/Error res)
                       (clojure.core/tagged-literal 'error
                         {:type (.-name res)
                          :message (.-message res)
                          :trace (clojure.core/->> res .-stack clojure.string/split-lines)})

                       (clojure.core/symbol? res)
                       (clojure.core/symbol (clojure.core/str "#unrepl/bad-symbol [nil "
                                                 (clojure.core/pr-str (clojure.core/str res))
                                                 "]"))

                       (clojure.core/and change-keywords? (clojure.core/keyword? res))
                       (clojure.core/symbol (clojure.core/str "#unrepl/bad-keyword ["
                                                 (clojure.core/pr-str (clojure.core/namespace res)) " "
                                                 (clojure.core/pr-str (clojure.core/name res))
                                                 "]"))

                       :else res))]
  (try
    (clojure.core/let [res (do __COMMAND__)]
      [:result (clojure.core/pr-str
                (if (clojure.core/record? res)
                  (clojure.walk/postwalk (clojure.core/fn [a] (res-fn a false)) res)
                  (if (clojure.core/coll? res)
                    (clojure.walk/postwalk (clojure.core/fn [a] (res-fn a true)) res)
                    (res-fn res true))))])
    (catch :default e [:error (clojure.core/pr-str (res-fn e true))])))
