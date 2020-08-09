(try
  (clojure.core/let [res (do __COMMAND__)
                     res (clojure.core/cond
                            #?(:cljs false :clje false :default (clojure.core/ratio? res))
                            (clojure.core/symbol
                             (clojure.core/str "#repl-tooling/literal-render \""
                                  (clojure.core/pr-str res) "\""))

                            (clojure.core/var? res)
                            (clojure.core/symbol
                             (clojure.core/str "#repl-tooling/literal-render \""
                                  (clojure.core/pr-str res) "\""))

                            (clojure.core/symbol? res)
                            (clojure.core/symbol (clojure.core/str "#unrepl/bad-symbol [nil "
                                                      (clojure.core/pr-str (clojure.core/str res))
                                                      "]"))

                            (clojure.core/keyword? res)
                            (clojure.core/symbol (clojure.core/str "#unrepl/bad-keyword ["
                                                      (clojure.core/pr-str (clojure.core/namespace res)) " "
                                                      (clojure.core/pr-str (clojure.core/name res))
                                                      "]"))

                            (clojure.core/->> res clojure.core/type clojure.core/str (clojure.core/re-find #"Big(Decimal|Float)"))
                            (clojure.core/symbol (clojure.core/str "#unrepl/bigdec "res))

                            (clojure.core/->> res clojure.core/type clojure.core/str (clojure.core/re-find #"BigInt"))
                            (clojure.core/symbol (clojure.core/str "#unrepl/bigint "res))

                            :else res)]
    ['tooling$eval-res '__ID__ {:result (clojure.core/pr-str res)
                                :as-text (clojure.core/pr-str res)}])
  (catch __EX_TYPE__ e
    (clojure.core/let [ex #?(:cljr (clojure.string/replace (clojure.core/pr-str e) #":message \n" "")
                             :default (clojure.core/pr-str e))]
      ['tooling$eval-res '__ID__ {:error ex
                                  :as-text ex}])))
