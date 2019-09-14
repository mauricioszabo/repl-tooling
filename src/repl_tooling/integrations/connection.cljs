(ns repl-tooling.integrations.connection
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [cljs-blob-contents]])
  (:require [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.repl-client :as client]
            [repl-tooling.features.shadow-cljs :as shadow-cljs]))

(def blob (cljs-blob-contents))

(defn- treat-result [evaluator resolve ret]
  (if (:error ret)
    (do (resolve ret) (client/disconnect! evaluator))
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
                               (client/disconnect! evaluator)))))))

(defn ^:deprecated auto-connect-embedded!
  "Given a host, port, and project paths, try to parse shadow-cljs.edn and
connects to the first build-id found on file. Returns an evaluator for CLJS

Callbacks expects :on-stdout and :on-stderr"
  [host port project-paths callbacks]
  (let [code (shadow-cljs/command-for project-paths)
        repl (delay (clj-repl/repl :cljs-aux host port
                                   #(do
                                      (cond
                                       (or (contains? % :result) (contains? % :error))
                                       ((:on-result callbacks) (helpers/parse-result %))

                                       (:out %)
                                       ((:on-stdout callbacks) (:out %))))))]
    (js/Promise.
     (fn [resolve]
       (if (:error code)
         (resolve code)
         (.. (clj-repl/self-host @repl code)
             (then #(treat-result @repl resolve %))))))))

(defn connect!
  "Given a host, port, and a clojure command, connects on a Clojure REPL,
runs the command to change it to CLJS, and returns an evaluator for CLJS.
Callbacks expects :on-stdout and :on-result"
  [host port code callbacks]
  (let [repl (delay (clj-repl/repl :cljs-aux host port
                                   #(do
                                      (cond
                                       (or (contains? % :result) (contains? % :error))
                                       ((:on-result callbacks) (helpers/parse-result %))

                                       (:out %)
                                       ((:on-stdout callbacks) (:out %))))))]
    (js/Promise. (fn [resolve]
                   (if (:error code)
                     (resolve code)
                     (.. (clj-repl/self-host @repl code)
                         (then #(treat-result @repl resolve %))))))))
