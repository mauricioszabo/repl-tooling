(ns repl-tooling.features.shadow-cljs
  (:require [cljs.reader :as edn]
            ["fs" :as fs]
            ["path" :as path]))

(def ^private fs (js/require "fs"))
(def ^private exists-sync (.-existsSync fs))
(def ^private read-file (.-readFileSync fs))

(defn- readfile [shadow-path]
  (-> shadow-path read-file str edn/read-string
      :builds first first))

(defn- cmd-for [shadow-path]
  (let [build-id (readfile shadow-path)]
    `(do
       (~'clojure.core/require '[shadow.cljs.devtools.api])
       (shadow.cljs.devtools.api/repl ~build-id))))

(defn command-for [project-folders]
  (let [first-shadow-file (->> project-folders
                               (map #(path/join % "/shadow-cljs.edn"))
                               (filter exists-sync)
                               first)]
    (if first-shadow-file
      (cmd-for first-shadow-file)
      {:error :no-shadow-file})))
