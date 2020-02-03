(defn evaluate [build-id command]
  (clojure.core/let [res
                     (shadow.cljs.devtools.server.worker/worker-request
                         (shadow.cljs.devtools.api/get-worker build-id)
                         {:type :repl-eval :input command})]
    (clojure.core/case (:type res)
      :repl/error (throw (:ex res))

      :repl/interrupt
      nil

      :repl/timeout
      (throw (ex-info "Timeout while waiting for REPL result." {}))

      :repl/no-runtime-connected
      (throw (ex-info "No application has connected to the REPL server. Make sure your JS environment has loaded your compiled ClojureScript code." {}))

      :repl/too-many-runtimes
      (throw (ex-info (println "There are too many connected processes." {})))

      :repl/worker-stop
      (throw (ex-info "The Shadow-CLJS worker has stopped." {}))

      (clojure.core/-> res
                       :results
                       clojure.core/last
                       :result
                       :value))))
