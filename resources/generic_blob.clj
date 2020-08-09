(try
  (clojure.core/let [res (do __COMMAND__)
                     res (___repl-tooling.__generic_printer_blob/serialize res)]
    ['tooling$eval-res '__ID__ {:result (clojure.core/pr-str res)
                                :as-text (clojure.core/pr-str res)}])
  (catch __EX_TYPE__ e
    (clojure.core/let [ex (___repl-tooling.__generic_printer_blob/serialize e)
                       ex #?(:clje (clojure.core/tagged-literal
                                    'error
                                    {:type "Error"
                                     :message ex
                                     :trace (clojure.core/mapv
                                             (clojure.core/fn [[_ _ _ [[_ file] [_ line]]]]
                                                [nil nil (clojure.core/str file) line])
                                             (erlang/get_stacktrace))})
                             :default ex)]
      ['tooling$eval-res '__ID__ {:error (clojure.core/pr-str ex)
                                  :as-text (clojure.core/pr-str ex)}])))
