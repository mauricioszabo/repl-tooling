(ns repl-tooling.integrations.connection
  (:require [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client :as client]
            [repl-tooling.features.shadow-cljs :as shadow-cljs]))

(defn- treat-result [evaluator resolve ret]
  (if (:error ret)
    (do (resolve ret) (client/disconnect! evaluator))
    (eval/evaluate ret
                   "(/ 10 0)"
                   {}
                   (fn [{:keys [error result]}]
                     (cond
                       (= result "##Inf") (resolve ret)
                       :else (do
                               (resolve {:error :unknown})
                               (client/disconnect! evaluator)))))))

(defn auto-connect-embedded! [host port project-paths]
  (let [code (shadow-cljs/command-for project-paths)
        repl (delay (clj-repl/repl :clj-aux host port #(prn [:CLOJURESCRIPT-RETURN %])))]

    (js/Promise.
     (fn [resolve]
       (if (:error code)
         (resolve code)
         (.. (clj-repl/self-host @repl code)
             (then #(treat-result @repl resolve %))))))))
