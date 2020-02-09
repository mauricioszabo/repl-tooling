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

(defn watch-events [build-id patch-cmd]
  (clojure.core/require '[clojure.core.async])
  (clojure.core/require '[cljs.reader])

  (clojure.core/let [c (clojure.core.async/chan
                         (clojure.core.async/sliding-buffer 8000))]
    (shadow.cljs.devtools.server.worker/watch
     (shadow.cljs.devtools.api/get-worker build-id)
     c
     true)
    (clojure.core.async/go-loop []
      (.setUncaughtExceptionHandler
       (Thread/currentThread)
       (clojure.core/reify Thread$UncaughtExceptionHandler (uncaughtException [_ _ _]
                                                             (clojure.core.async/close! c))))
      (clojure.core/when-let [res (clojure.core.async/<! c)]
        (clojure.core/when (clojure.core/and (clojure.core/= :repl/result (:type res))
                                             (:patch (:result res)))
          (patch-cmd (:id (:result res)) (:patch (:result res))))
        (clojure.core/when (clojure.core/= :repl/out (:type res))
          (clojure.core/println (:text res)))
        (clojure.core/when (clojure.core/and (clojure.core/= :repl/action (:type res))
                                             (:warnings (:action res)))
          (clojure.core/binding [clojure.core/*out* clojure.core/*err*]
            (clojure.core/doseq [{:keys [msg]} (:warnings (:action res))]
              (clojure.core/println "WARNING:" msg))))
        (recur)))))
