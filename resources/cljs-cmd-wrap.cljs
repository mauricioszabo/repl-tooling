(clojure.core/let [tooling$norm$walk (clojure.core/atom nil)

                   tooling$norm$jsbeam
                   (clojure.core/fn [js-obj]
                     (clojure.core/tagged-literal
                      'unrepl/browsable
                      [(if (clojure.core/= js/Function (clojure.core/type js-obj))
                         (clojure.core/let [fn-name (clojure.core/-> js-obj .-name cljs.core/demunge)
                                            fn-name (if (clojure.core/empty? fn-name)
                                                      (clojure.core/pr-str js-obj)
                                                      fn-name)]
                           (clojure.core/tagged-literal 'unrepl/bad-symbol
                                                          [nil
                                                           (clojure.core/str
                                                            fn-name
                                                            " (function)")]))
                         (if (try (cljs.reader/read-string {:default clojure.core/tagged-literal}
                                                           (clojure.core/pr-str js-obj))
                               (catch :default _ nil))
                           js-obj
                           (clojure.core/tagged-literal 'unrepl/bad-symbol [nil (clojure.core/pr-str js-obj)])))
                       {:repl-tooling/... `(quote
                                             ~(->> js-obj
                                                  js/Object.getPrototypeOf
                                                  js/Object.getOwnPropertyNames
                                                  (clojure.core/concat (js/Object.getOwnPropertyNames js-obj))
                                                  distinct
                                                  sort
                                                  (clojure.core/map #(clojure.core/symbol (str "." %)))))}]))

                   res-fn
                   (clojure.core/fn [res]
                     (clojure.core/cond
                       (clojure.core/= "cljs.core/Atom" (clojure.core/pr-str (clojure.core/type res)))
                       (clojure.core/tagged-literal 'atom
                         (@tooling$norm$walk @res))

                       ; Reagent fixes...
                       (clojure.core/= "reagent.ratom/RAtom" (clojure.core/pr-str (clojure.core/type res)))
                       (clojure.core/tagged-literal 'reagent.ratom/RAtom
                         (@tooling$norm$walk @res))

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
                       (if (clojure.core/re-find (clojure.core/re-pattern "\\s") (str res))
                        (clojure.core/symbol (clojure.core/str "#unrepl/bad-symbol [nil "
                                                  (clojure.core/pr-str (clojure.core/str res))
                                                  "]"))
                        res)

                       (clojure.core/keyword? res)
                       (if (clojure.core/re-find (clojure.core/re-pattern "\\s") (str res))
                        (clojure.core/symbol (clojure.core/str "#unrepl/bad-keyword ["
                                               (clojure.core/pr-str (clojure.core/namespace res)) " "
                                               (clojure.core/pr-str (clojure.core/name res))
                                               "]"))
                        res)

                       (clojure.core/instance? js/Promise res)
                       (clojure.core/let [id (clojure.core/gensym "patch")]
                         (.then res
                                (clojure.core/fn [resolved]
                                  (cljs.core/tap>
                                   (clojure.core/tagged-literal
                                    'repl-tooling/patch
                                    [id
                                     (clojure.core/pr-str
                                      (clojure.core/tagged-literal
                                       'promise
                                       (@tooling$norm$walk resolved)))]))))
                         (clojure.core/tagged-literal
                          'repl-tooling/patchable [id (clojure.core/tagged-literal 'promise '<pending>)]))

                       ;; Collections...
                       (clojure.core/map? res)
                       (clojure.core/let [norm (clojure.core/->> res
                                                                 (clojure.core/map #(clojure.core/mapv @tooling$norm$walk %))
                                                                 (clojure.core/into {}))]
                         (if (clojure.core/record? res)
                           (clojure.core/tagged-literal
                            (clojure.core/symbol (clojure.core/pr-str (clojure.core/type res)))
                            norm)
                           norm))

                       (clojure.core/vector? res)
                       (clojure.core/mapv @tooling$norm$walk res)

                       (clojure.core/set? res)
                       (clojure.core/->> res (clojure.core/map @tooling$norm$walk) (clojure.core/into #{}))

                       (clojure.core/coll? res)
                       (clojure.core/map @tooling$norm$walk res)

                       (clojure.core/keyword? res) res
                       (clojure.core/= nil res) res
                       (clojure.core/boolean? res) res
                       (clojure.core/number? res) res
                       (clojure.core/string? res) res
                       (clojure.core/regexp? res) res
                       :else (tooling$norm$jsbeam res)))]
  (try
    (clojure.core/reset! tooling$norm$walk (clojure.core/memoize res-fn))
                        ; (clojure.core/fn [res]
                          ; (if (clojure.core/record? res)
                          ;   (clojure.walk/postwalk (clojure.core/fn [a] (res-fn a false)) res)
                            ; (if (clojure.core/coll? res)
                            ;   (clojure.walk/postwalk (clojure.core/fn [a] (res-fn a)) res)
                              ; (res-fn res))

    (clojure.core/let [res (do __COMMAND__)]
      ['tooling$eval-res '__ID__ {:result (clojure.core/pr-str (@tooling$norm$walk res))
                                  :as-text (clojure.core/pr-str (@tooling$norm$walk res))}])
    (catch :default e ['tooling$eval-res '__ID__ {:error (clojure.core/pr-str (@tooling$norm$walk e))
                                                  :as-text (clojure.core/pr-str (@tooling$norm$walk e))}])))
