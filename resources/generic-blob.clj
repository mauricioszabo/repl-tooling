(try
  (clojure.core/let [res __COMMAND__
                     res (clojure.core/cond
                            #?(:cljs false :default (clojure.core/ratio? res))
                            (clojure.core/symbol
                             (clojure.core/str "#repl-tooling/literal-render \""
                                  (clojure.core/pr-str res) "\""))

                            (clojure.core/symbol? res)
                            (clojure.core/symbol (clojure.core/str "#unrepl/bad-symbol [nil "
                                                      (clojure.core/pr-str (clojure.core/str res))
                                                      "]"))

                            (clojure.core/keyword? res)
                            (clojure.core/symbol (clojure.core/str "#unrepl/bad-keyword ["
                                                      (clojure.core/pr-str (#?(:bb namespace :clj clojure.core/namespace) res)) " "
                                                      (clojure.core/pr-str (clojure.core/name res))
                                                      "]"))

                            (clojure.core/->> res type str (clojure.core/re-find #"BigDecimal"))
                            (clojure.core/symbol (clojure.core/str "#unrepl/bigdec "res))

                            (clojure.core/->> res type str (clojure.core/re-find #"BigIng"))
                            (clojure.core/symbol (clojure.core/str "#unrepl/bigint "res))

                            :else res)]
    ['tooling$eval-res '__ID__ {:result (clojure.core/pr-str res)}])
  (catch __EX_TYPE__ e
    ['tooling$eval-res '__ID__ {:error (clojure.core/pr-str e)}]))
