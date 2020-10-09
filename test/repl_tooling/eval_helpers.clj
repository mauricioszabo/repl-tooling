(ns repl-tooling.eval-helpers
  (:require [clojure.core.async :as async]
            [check.async-old :refer [async-test]]))

(defmacro eval-on-repl
  "Helper to evaluate code on the repl, and don't parse. Expect you to have
a variable `repl` that points to the evaluator"
  [code]
  `(let [result# (async/promise-chan)]
     (repl-tooling.eval/evaluate ~'repl ~code {} (fn [res#] (async/put! result# res#)))
     (async/<! result#)))

(defn- prepare-repl [repl-name body conn-id out-chan disable?]
  `(let [prom# (repl-tooling.integrations.repls/connect-repl!
                ~conn-id "localhost" 2233 #(some->> % (async/put! ~'out)))
         c# (async/promise-chan)
         _# (.then prom# #(async/put! c# (second %)))
         ~repl-name (async/<! c#)
         res1# (async/promise-chan)
         res2# (async/promise-chan)]
     (.then (repl-tooling.eval/eval ~repl-name ":ok") #(async/put! res1# :ok))
     (async/<! res1#)
     (when ~disable?
       (repl-tooling.repl-client.clojure/disable-limits! ~repl-name))
     (.then (repl-tooling.eval/eval ~repl-name ":done") #(async/put! res2# :ok))
     (async/<! res2#)
     ~@body))

(defmacro async-with-clj-repl [ txt & body]
  (let [conn-id (str (gensym "connection"))
        aux-id (str (gensym "connection"))]
    `(let [~'out (async/chan)
           ~'out-aux (async/chan)]
      (async-test ~txt {:timeout 8000
                        :teardown (do
                                    (async/close! ~'out)
                                    (async/close! ~'out-aux)
                                    (repl-tooling.integrations.repls/disconnect! ~conn-id)
                                    (repl-tooling.integrations.repls/disconnect! ~aux-id))}
        ~(prepare-repl 'aux `(let [#_ :ok]
                               ~(prepare-repl 'repl body conn-id 'out false))
                       aux-id 'out-aux true)))))

(defmacro async-with-repl [ txt & body]
  (let [conn-id (str (gensym "connection"))]
    `(let [~'out (async/chan)]
      (async-test ~txt {:timeout 8000
                        :teardown (do
                                    (async/close! ~'out)
                                    (repl-tooling.integrations.repls/disconnect! ~conn-id))}
       ~(prepare-repl 'repl body conn-id 'out true)))))

(defmacro async-with-cljs-repl [ txt & body]
  (let [conn-id (str (gensym "connection"))
        aux-id (str (gensym "connection"))]
    `(let [~'out (async/chan)
           ~'out-aux (async/chan)]
      (async-test ~txt {:timeout 8000
                        :teardown (do
                                    (async/close! ~'out)
                                    (async/close! ~'out-aux)
                                    (repl-tooling.integrations.repls/disconnect! ~conn-id)
                                    (repl-tooling.integrations.repls/disconnect! ~aux-id))}
        (let [prom# (repl-tooling.integrations.connection/connect-shadow-ws!
                     {:identifier ~conn-id :host "localhost" :port 2233
                      :build-id :fixture
                      :on-stdout #(some->> % (async/put! ~'out))
                      :directories ["."]
                      :on-result #()})
              c# (async/promise-chan)
              _# (.then prom# #(async/put! c# %))
              ~'repl (async/<! c#)
              res1# (async/promise-chan)]
          (async/<! (async/timeout 100))
          (.then (repl-tooling.eval/eval ~'repl ":done") #(async/put! res1# :ok))
          (async/<! res1#)
          ~(prepare-repl 'aux body aux-id 'out-aux true))))))

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
