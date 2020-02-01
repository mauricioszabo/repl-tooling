(try
  (clojure.core/let [res (do __COMMAND__)
                     res (clojure.core/cond
                            (clojure.core/symbol? res)
                            (clojure.core/symbol (clojure.core/str "#unrepl/bad-symbol [nil "
                                                      (clojure.core/pr-str (clojure.core/str res))
                                                      "]"))

                            (clojure.core/keyword? res)
                            (clojure.core/symbol (clojure.core/str "#unrepl/bad-keyword ["
                                                      (clojure.core/pr-str (clojure.core/namespace res)) " "
                                                      (clojure.core/pr-str (clojure.core/name res))
                                                      "]"))

                            :else res)]
    [:result (clojure.core/pr-str res)])
  (catch :default e [:error (clojure.core/pr-str e)]))
