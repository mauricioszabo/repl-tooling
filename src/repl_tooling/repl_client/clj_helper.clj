(ns repl-tooling.repl-client.clj-helper
  (:require [clojure.java.io :as io]
            [clojure.tools.reader :as r]
            [clojure.test :refer [testing]]
            [clojure.string :as str]))

(defmacro contents-for-fn
  ([source-file] (slurp (io/resource source-file)))
  ([source-file fn-name]
   (let [re (re-pattern (str "(?s).*\\(defn\\s+(" fn-name "\\b)"))]
     (-> source-file
         io/resource
         slurp
         (str/replace re "(clojure.core/fn $1 ")
         r/read-string
         str))))

(defmacro blob-contents []
  (slurp (io/resource "unrepl.clj")))

(defmacro cljs-blob-contents []
  (slurp (io/resource "cljs-blob.cljs")))

(defmacro generic-blob-contents []
  (slurp (io/resource "generic_printer_blob.clj")))

(defmacro generic-eval-wrapper []
  (slurp (io/resource "generic_blob.clj")))
