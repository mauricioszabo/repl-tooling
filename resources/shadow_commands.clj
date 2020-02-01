(defn evaluate [build-id command]
  (clojure.core/let [res
                     (shadow.cljs.devtools.server.worker/worker-request
                         (shadow.cljs.devtools.api/get-worker build-id)
                         {:type :repl-eval :input command})]
    (if (clojure.core/= :repl/error (:type res))
      (throw (:ex res))
      (clojure.core/-> res
                       :results
                       clojure.core/last
                       :result
                       :value))))
