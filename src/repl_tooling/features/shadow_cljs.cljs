(ns repl-tooling.features.shadow-cljs
  (:require [cljs.reader :as edn]
            ["path" :as path]
            ["fs" :refer [existsSync readFileSync]]))

(defn- readfile [shadow-path]
  (->> shadow-path readFileSync str
      (edn/read-string {:default tagged-literal})
      :builds keys))

(defn cmd-for [build-id]
  `(do
     (~'clojure.core/require '[shadow.cljs.devtools.api])
     (shadow.cljs.devtools.api/repl ~build-id)))

(defn- cmds-for [shadow-path]
  (->> (readfile shadow-path)
       (map (juxt identity cmd-for))
       (into {})))

; TODO: Move this to another NS, or maybe detect better
(defn command-for [project-paths]
  (let [first-shadow-file (->> project-paths
                               (map #(path/join % "shadow-cljs.edn"))
                               (filter existsSync)
                               first)]
    (if first-shadow-file
      (cmds-for first-shadow-file)
      {:error :no-shadow-file})))
