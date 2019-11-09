(ns repl-tooling.repl-client.clj-helper
  (:require [clojure.java.io :as io]))

(defmacro blob-contents []
  (slurp (io/resource "unrepl.clj")))

(defmacro cljs-blob-contents []
  (slurp (io/resource "cljs-blob.cljs")))

(defmacro generic-eval-wrapper []
  (slurp (io/resource "generic-blob.clj")))
