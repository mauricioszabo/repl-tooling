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

(defmacro wait-for-change [fn-to-change]
  `(let [old-val# (~fn-to-change)
         res# (async/go-loop [tries# 0]
                (let [new-val# (~fn-to-change)]
                  (cond
                    (or (not= new-val# old-val#) (>= tries# 20)) new-val#
                    (= new-val# old-val#) (do
                                            (async/<! (async/timeout 20))
                                            (recur (inc tries#))))))]
     (async/<! res#)))
