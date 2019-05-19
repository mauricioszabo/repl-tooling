(ns repl-tooling.eval-helpers
  (:require [clojure.core.async :as async]))

(defmacro eval-on-repl
  "Helper to evaluate code on thh repl, and don't parse. Expect you to have
a variable `repl` that points to the evaluator"
  [code]
  `(let [result# (async/promise-chan)]
    (repl-tooling.eval/evaluate ~'repl ~code {} (fn [res#] (async/put! result# res#)))
    (async/<! result#)))

(defmacro eval-and-parse [code]
  `(repl-tooling.editor-helpers/parse-result (eval-on-repl ~code)))
