(ns repl-tooling.repl-client.clojure-test
  (:require [clojure.test :refer-macros [testing run-tests]]
            [check.core :refer-macros [check]]
            [check.async-cljs :refer-macros [def-async-test await!]]
            [cljs.core.async :refer-macros [go go-loop] :as async]
            [repl-tooling.repl-client :as client]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj]))

(def-async-test "Evaluate a request-response test"
  {:teardown (client/disconnect! :clj-test1)}
  (let [out (async/chan)
        repl (clj/repl :clj-test1 "localhost" 5555 #(async/put! out %))]
    (testing "evaluating resquest-response"
      (eval/evaluate repl "(+ 1 2)" {} #(async/put! out %))
      (check (await! out) => {:result "3"})
      (check (await! out) => "3"))

    (testing "capturing output"
      (eval/evaluate repl "(println :foobar)" {} #(async/put! out %))
      (check (await! out) => {:out ":foobar\n"})
      (check (await! out) => {:result "nil"})
      (check (await! out) => "nil"))))

    ; (testing "breaks long-running evaluations"
    ;   (let [id (eval/evaluate repl "(do (Thread/sleep 1000) :foo)" {}
    ;                           #(async/put! out %))]
    ;     (await! (async/timeout 100))
    ;     (eval/break repl id))
    ;   (eval/evaluate repl ":bar" {} #(async/put! out %))
    ;   (check (await! out) => {:result ":bar"})
    ;   (check (await! out) => ":bar"))))

(run-tests)
