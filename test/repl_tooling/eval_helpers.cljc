(ns repl-tooling.eval-helpers
  (:require [clojure.core.async :as async]))

(defmacro eval-on-repl [code]
  `(let [result# (async/promise-chan)]
    (repl-tooling.eval/evaluate ~'repl ~code {} (fn [res#] (async/put! result# res#)))
    (async/<! result#)))

(defmacro eval-and-parse [code]
  `(repl-tooling.editor-helpers/parse-result (eval-on-repl ~code)))
