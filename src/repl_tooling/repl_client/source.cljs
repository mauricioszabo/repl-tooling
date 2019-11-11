(ns repl-tooling.repl-client.source
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [generic-eval-wrapper]])
  (:require [clojure.string :as str]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]))

(declare normalize-command)
(defn- conv-node [node]
  (cond
    (or (node/whitespace-or-comment? node)
        (node/linebreak? node))
    (node/whitespace-node " ")

    :else
    (normalize-command node)))

(defn- normalize-command [command]
  (cond-> command (contains? command :children) (update :children #(map conv-node %))))

(defn parse-command [command remove-lines?]
  (let [command (str command)
        cmd (try
              {:result (parser/parse-string-all command)}
              (catch :default e
                {:error (pr-str (.-message e))}))]
    (if-let [res (:result cmd)]
      {:result (str (cond-> res remove-lines? normalize-command))}
      cmd)))

(def ^:private template (generic-eval-wrapper))
(defn wrap-command [id cmd ex-type strip-newlines?]
  (let [cmd (parse-command cmd strip-newlines?)]
    (if-let [res (:result cmd)]
      (-> template
          (str/replace-all #"__COMMAND__" (str res "\n"))
          (str/replace-all #"__ID__" id)
          (str/replace-all #"__EX_TYPE__" ex-type)
          (parse-command strip-newlines?)
          (update :result str "\n"))
      cmd)))