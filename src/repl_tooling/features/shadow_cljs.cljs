(ns repl-tooling.features.shadow-cljs
  (:require-macros [repl-tooling.repl-client.clj-helper :as h])
  (:require [cljs.reader :as edn]
            [clojure.string :as str]
            ["path" :as path]
            ["fs" :refer [existsSync readFileSync]]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.repl-client.source :as source]
            [repl-tooling.editor-helpers :as helpers]
            [promesa.core :as p]))

(defn- readfile [shadow-path]
  (-> shadow-path readFileSync str edn/read-string
      :builds keys))

(defn cmd-for [build-id]
  `(do
     (~'clojure.core/require '[shadow.cljs.devtools.api])
     (shadow.cljs.devtools.api/repl ~build-id)))

(defn- cmds-for [shadow-path]
  (->> (readfile shadow-path)
       (map (juxt identity cmd-for))
       (into {})))

(defn command-for [project-paths]
  (let [first-shadow-file (->> project-paths
                               (map #(path/join % "shadow-cljs.edn"))
                               (filter existsSync)
                               first)]
    (if first-shadow-file
      (cmds-for first-shadow-file)
      {:error :no-shadow-file})))

(defn commands-for [repl]
  (let [cmd "(do
                (clojure.core/require 'shadow.cljs.devtools.api)
                (clojure.core/require 'shadow.cljs.devtools.server.worker))
                (clojure.core/filter shadow.cljs.devtools.api/worker-running?
                                     (shadow.cljs.devtools.api/get-build-ids)))"]
    (.. (eval/eval repl cmd)
        (then #(if (not-empty %)
                 (->> %
                       :result
                       (map (juxt identity cmd-for))
                       (into {}))
                 {:error :workers-empty}))
        (catch #(hash-map :error :no-shadow)))))

(def cmd-for-shadow (h/contents-for-fn "shadow_commands.clj" "evaluate"))
(defn- parse-shadow-res [callback result]
  (if (contains? result :error)
    (callback result)
    (let [parsed (helpers/parse-result (select-keys result [:as-text :result]))
          [key val] (-> parsed
                        (assoc :as-text (or (:result parsed) (:error parsed)))
                        (dissoc :parsed?)
                        helpers/parse-result
                        :result)]
      (callback (-> result
                    (dissoc :result :error)
                    (assoc :as-text val key val))))))

(def wrapped-cmd (h/contents-for-fn "cljs-cmd-wrap.cljs"))
(defrecord Shadow [clj-evaluator build-id]
  eval/Evaluator
  (evaluate [self command opts callback]
    (let [id (or (:id opts) (gensym))
          clj-opts (dissoc opts :namespace)
          name-space (:namespace opts)
          code (str/replace wrapped-cmd #"__COMMAND__" (str command "\n"))
          clj-cmd (str "(" cmd-for-shadow " " build-id " " (pr-str code) ")")]

      (eval/evaluate clj-evaluator clj-cmd clj-opts #(parse-shadow-res callback %))
      id))

  (break [this repl]))

(defn upgrade-repl! [repl build-id]
  (.. (clj-repl/disable-limits! repl)
      (then #(->Shadow repl build-id))))
