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

(def-async-test "Captures specific UnREPL outputs"
  {:teardown (client/disconnect! :clj-test2)}
  (let [out (async/chan)
        repl (clj/repl :clj-test2 "localhost" 5555 identity)
        res #(async/put! out %)]

    (testing "capturing JAVA classes"
      (eval/evaluate repl "Throwable" {} res)
      (check (await! out) => {:result "java.lang.Throwable"}))

    (testing "capturing exceptions"
      (eval/evaluate repl "(/ 20 0)" {} res)
      (check (:error (await! out)) => #"Divide by zero"))

    (testing "capturing big data"
      (eval/evaluate repl "(range)" {} res)
      (let [r (await! out)]
        (check (:result r) => #"(0 1 2 3 4.*)")
        (check (:as-text r) => '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ...))

        (eval/evaluate repl (-> r :as-text last meta :get-more) {} res)
        (check (:result (await! out)) => #"(10 11 12 13 14.*)")))))

(run-tests)

; (comment
;   (client/disconnect! :clj-test)
;   (def repl (clj/repl :clj-test "localhost" 5555 identity))
;   (eval/evaluate repl "Throwable\n" {} #(prn [:OUT %]))
;   (eval/evaluate repl "(println 10)" {} #(prn [:OUT %])))