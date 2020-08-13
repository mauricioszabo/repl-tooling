(ns repl-tooling.integrations.connection
  (:require [promesa.core :as p]
            [repl-tooling.repl-client.clj-helper :refer [cljs-blob-contents]]
            [cljs.reader :as edn]
            [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.features.shadow-cljs :as shadow-cljs]
            [repl-tooling.repl-client.shadow-ws :as shadow-ws]
            [repl-tooling.integrations.repls :as repls]
            ["fs" :refer [readFileSync existsSync]]
            ["path" :refer [join]]))

(def blob (cljs-blob-contents))

(defn- treat-result [id resolve ret]
  (if (:error ret)
    (do (resolve ret) (repls/disconnect! id))
    (eval/evaluate ret
                   "(/ 10 0)"
                   {:ignore true}
                   (fn [{:keys [result]}]
                     (cond
                       (= result "##Inf") (do
                                            (eval/evaluate ret blob
                                                           {:ignore true}
                                                           identity)
                                            (resolve ret))
                       :else (do
                               (resolve {:error :unknown})
                               (repls/disconnect! id)))))))

(defn connect-self-hosted!
  "Given a host, port, and a clojure command, connects on a Clojure REPL,
runs the command to change it to CLJS, and returns an evaluator for CLJS."
  [{:keys [identifier host port code on-result on-stdout]
    :or {identifier :cljs-eval}}]
  (p/let [repl-info (delay (repls/connect-repl! identifier host port
                                                (fn [res]
                                                  (cond
                                                    (or (contains? res :result)
                                                        (contains? res :error))
                                                    (on-result (helpers/parse-result res))

                                                    (:out res)
                                                    (on-stdout (:out res))))))]
    (if (:error code)
      code
      (p/let [repl-info @repl-info
              [_ clj-repl] repl-info
              self-hosted (clj-repl/self-host clj-repl code)]
        (js/Promise. (fn [resolve] (treat-result identifier resolve self-hosted)))))))

(defn connect-shadow-ws!
  [{:keys [identifier build-id on-stdout on-stderr on-patch directories compile-error
           on-eval on-start-eval]
    :or {identifier :cljs-eval}}]
  (let [host "localhost"
        dir (->> directories
                 (filter #(existsSync (join % ".shadow-cljs" "http.port")))
                 first)
        port (-> (join dir ".shadow-cljs" "http.port") readFileSync str js/parseInt)
        token (-> (join dir ".shadow-cljs" "server.token") readFileSync str)
        on-output (fn [res]
                    (cond
                      (:result res)
                      (let [res (:result res)]
                        (on-start-eval (dissoc res :result))
                        (on-eval (-> res
                                     (update :result helpers/parse-result)
                                     (assoc :repl nil))))

                      (:out res)
                      (on-stdout (:out res))

                      (:err res)
                      (on-stderr (:err res))

                      (:compile-err res)
                      (compile-error (:compile-err res))

                      (:patch res)
                      (on-patch (:patch res))))]

    (shadow-ws/connect! {:id identifier
                         :build-id build-id
                         :host host
                         :port port
                         :token token
                         :on-output on-output})))
