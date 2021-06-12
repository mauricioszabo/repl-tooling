(try
  (clojure.core/let [res (do __COMMAND__)
                     res (___repl-tooling.__generic_printer_blob/serialize res)]
    ['tooling$eval-res '__ID__ {:result (clojure.core/pr-str res)
                                :as-text (clojure.core/pr-str res)}])
  (catch __EX_TYPE__ e #?(:clje :stack) #?(:clje st)
    (clojure.core/let [ex #?(:clje (clojure.core/tagged-literal
                                    'error
                                    (___repl-tooling.__generic_printer_blob/normalize-error e st))
                             :default (___repl-tooling.__generic_printer_blob/serialize e))]
      ['tooling$eval-res '__ID__ {:error (clojure.core/pr-str ex)
                                  :as-text (clojure.core/pr-str ex)}])))
