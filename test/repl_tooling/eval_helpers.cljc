(ns repl-tooling.eval-helpers
  (:require [clojure.core.async :as async]
            [check.async :include-macros true :refer [async-test]]
            #?(:cljs [repl-tooling.integrations.repls :as repls])
            #?(:cljs [repl-tooling.repl-client.clojure :as clj-repl])))
                    ; [repl-tooling.repl-client :as client]


(defmacro eval-on-repl
  "Helper to evaluate code on the repl, and don't parse. Expect you to have
a variable `repl` that points to the evaluator"
  [code]
  `(let [result# (async/promise-chan)]
    (repl-tooling.eval/evaluate ~'repl ~code {} (fn [res#] (async/put! result# res#)))
    (async/<! result#)))

(defmacro async-with-repl [ txt & body]
  (let [conn-id (str (gensym "connection"))]
    `(let [~'out (async/chan)]
      (async-test ~txt {:timeout 8000
                        :teardown (fn []
                                    (async/close! ~'out)
                                    (repl-tooling.integrations.repls/disconnect! ~conn-id))}
        (let [prom# (repl-tooling.integrations.repls/connect-repl!
                      ~conn-id "localhost" 2233 #(some->> % (async/put! ~'out)))
              c# (async/promise-chan)
              _# (.then prom# #(async/put! c# (second %)))
              ~'repl (async/<! c#)]
          (eval-on-repl ":ok")
          (repl-tooling.repl-client.clojure/disable-limits! ~'repl)
          (eval-on-repl ":done")
          ~@body)))))

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
