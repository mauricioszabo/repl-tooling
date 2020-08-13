(ns repl-tooling.repl-client.source
  (:require [clojure.string :as str]
            [repl-tooling.repl-client.clj-helper :refer [generic-eval-wrapper]]
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

(def ^:private default-template (generic-eval-wrapper))
(defn wrap-command
  ([id cmd ex-type strip-newlines?]
   (wrap-command default-template id cmd ex-type strip-newlines?))
  ([template id cmd ex-type strip-newlines?]
   (let [cmd (parse-command cmd strip-newlines?)]
     (if-let [res (:result cmd)]
       (-> template
           (str/replace #"__COMMAND__" (str res "\n"))
           (str/replace #"__ID__" (pr-str id))
           (str/replace #"__EX_TYPE__" (pr-str ex-type))
           (parse-command strip-newlines?)
           (update :result str "\n"))
       cmd))))

(defn have-ns-command [ns-name]
  (str "(try (#?(:joker joker.core/require :default clojure.core/require) '" ns-name ") "
       "true "
       "(catch #?(:bb java.lang.Throwable :clj java.lang.Throwable "
       ":joker Error :cljs :default :cljr System.Exception :clje _) _ "
       "false))"))
