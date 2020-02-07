(ns repl-tooling.integrations.connection
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [cljs-blob-contents]])
  (:require [promesa.core :as p :include-macros true]
            [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.features.shadow-cljs :as shadow-cljs]
            [repl-tooling.integrations.repls :as repls]))

(def blob (cljs-blob-contents))

(defn- treat-result [id evaluator resolve ret]
  (if (:error ret)
    (do (resolve ret) (repls/disconnect! id))
    (eval/evaluate ret
                   "(/ 10 0)"
                   {:ignore true}
                   (fn [{:keys [error result]}]
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
        (js/Promise. (fn [resolve] (treat-result identifier clj-repl resolve self-hosted)))))))

(defn connect-shadow!
  "Given a host, port, and a clojure command, connects on a Clojure REPL and returns
an evaluator that will pipe all commands to Shadow-CLJS' workers."
  [{:keys [identifier host port build-id on-result on-stdout on-stderr]
    :or {identifier :cljs-eval}}]
  (p/let [[_ clj-repl] (repls/connect-repl! identifier host port
                                            (fn [res]
                                              (cond
                                                (or (contains? res :result)
                                                    (contains? res :error))
                                                (on-result (helpers/parse-result res))

                                                (:out res)
                                                (on-stdout (:out res))

                                                (:err res)
                                                (on-stderr (:err res)))))]
    (shadow-cljs/upgrade-repl! clj-repl build-id)))
