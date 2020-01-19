(ns repl-tooling.repl-client.clj-helper
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defmacro contents-for-fn [source-file fn-name]
  (let [re (re-pattern (str "(?s).*\\(defn\\s+(" fn-name "\\b)"))]
    (-> source-file
        io/resource
        slurp
        (str/replace re "(fn $1 ")
        (edn/read-string)
        str)))

(defmacro blob-contents []
  (slurp (io/resource "unrepl.clj")))

(defmacro cljs-blob-contents []
  (slurp (io/resource "cljs-blob.cljs")))

(defmacro generic-eval-wrapper []
  (slurp (io/resource "generic-blob.clj")))
