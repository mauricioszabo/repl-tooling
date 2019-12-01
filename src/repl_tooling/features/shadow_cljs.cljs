(ns repl-tooling.features.shadow-cljs
  (:require [cljs.reader :as edn]
            ["path" :as path]))

(def ^private fs ^js (js/require "fs"))
(def ^private exists-sync ^js (.-existsSync fs))
(def ^private read-file ^js (.-readFileSync fs))

(defn- readfile [shadow-path]
  (-> shadow-path read-file str edn/read-string
      :builds keys))

(defn- cmd-for [build-id]
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
                               (filter exists-sync)
                               first)]
    (if first-shadow-file
      (cmds-for first-shadow-file)
      {:error :no-shadow-file})))
