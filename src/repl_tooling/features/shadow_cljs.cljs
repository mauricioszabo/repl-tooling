(ns repl-tooling.features.shadow-cljs
  (:require [cljs.reader :as edn]
            ["fs" :as fs]
            ["path" :as path]))

(defn- readfile [shadow-path]
  (-> shadow-path fs/readFileSync str edn/read-string
      :builds first first))

(defn- cmd-for [shadow-path]
  (let [build-id (readfile shadow-path)]
    `(do
       (clojure.core/require '[shadow.cljs.devtools.api])
       (shadow.cljs.devtools.api/repl ~build-id))))

(defn command-for [project-folders]
  (let [first-shadow-file (->> project-folders
                               (map #(path/join % "/shadow-cljs.edn"))
                               (filter fs/existsSync)
                               first)]
    (if first-shadow-file
      (cmd-for first-shadow-file)
      {:error :no-shadow-file})))
