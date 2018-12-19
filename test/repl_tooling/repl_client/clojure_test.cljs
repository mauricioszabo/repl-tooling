(ns repl-tooling.repl-client.clojure-test
  (:require [clojure.test :refer-macros [testing run-tests]]
            [check.core :refer-macros [check]]
            [check.async-cljs :refer-macros [def-async-test await!]]
            [cljs.core.async :refer-macros [go go-loop] :as async]
            [repl-tooling.repl-client :as client]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj]))

; (def-async-test "Evaluate a request-response test"
;   {:teardown (client/disconnect! :clj-test1)}
;   (let [out (async/chan)
;         repl (clj/repl :clj-test1 "localhost" 5555 #(async/put! out %))]
;     (testing "evaluating resquest-response"
;       (eval/evaluate repl "(+ 1 2)" {} #(async/put! out %))
;       (check (await! out) =includes=> {:result "3"})
;       (check (await! out) =includes=> {:result "3" :as-text "3"}))
;
;     (testing "capturing output"
;       (eval/evaluate repl "(println :foobar)" {} #(async/put! out %))
;       (check (await! out) => {:out ":foobar\n"})
;       (check (await! out) =includes=> {:result "nil"})
;       (check (await! out) => {:result "nil" :as-text "nil"}))
;
;     (testing "passing parameters to evaluation"
;       (let [res (async/chan)]
;         (eval/evaluate repl "(/ 10 0)" {:filename "foo.clj" :row 12 :col 0}
;                        #(async/put! res (:error %)))
;         (check (await! res) => #"foo\.clj\" 12")))
;
;     (testing "breaking"
;       (let [res (async/chan)
;             id (eval/evaluate repl "(do (Thread/sleep 1000) :foo)" {}
;                               #(async/put! res %))]
;         (await! (async/timeout 100))
;         (testing "breaks evaliation"
;           (eval/break repl id)
;           (check (await! res) => {}))
;
;         (testing "allows new commands"
;           (eval/evaluate repl ":bar" {} #(async/put! res %))
;           (check (await! res) =includes=> {:result ":bar"}))))))
;
; (def-async-test "Captures specific UnREPL outputs"
;   {:teardown (client/disconnect! :clj-test2)}
;   (let [out (async/chan)
;         repl (clj/repl :clj-test2 "localhost" 5555 identity)
;         res #(async/put! out %)]
;
;     (testing "capturing JAVA classes"
;       (eval/evaluate repl "Throwable" {} res)
;       (check (await! out) =includes=> {:result "java.lang.Throwable"}))
;
;     (testing "capturing records"
;       (eval/evaluate repl "(do (defrecord Foo []) (->Foo))" {} res)
;       (let [r (await! out)]
;         (check r =includes=> {:result "{}" :as-text {}})
;         (check (-> r :as-text meta :tag) => "#user.Foo")))
;
;     (testing "capturing exceptions"
;       (eval/evaluate repl "(/ 20 0)" {} res)
;       (check (:error (await! out)) => #"Divide by zero"))
;
;     (testing "capturing big data"
;       (eval/evaluate repl "(range)" {} res)
;       (let [r (await! out)]
;         (check (:result r) => #"(0 1 2 3 4.*)")
;         (check (:as-text r) => '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ...))
;
;         (eval/evaluate repl (-> r :as-text last meta :get-more) {} res)
;         (check (:result (await! out)) => #"(10 11 12 13 14.*)")))
;
;     (testing "capturing big strings"
;       (eval/evaluate repl "(str (range 500))" {} res)
;       (let [r (await! out)]
;         (check (:result r) => #"(0 1 2 3 4.*)")
;         (check (pr-str (:as-text r)) => #"\(0 1 2 3 4 5.*\.\.\.")
;
;         (eval/evaluate repl (-> r :as-text meta :get-more) {} res)
;         (check (pr-str (:as-text (await! out))) => #"^\s?\d+.*\.\.\.")))))
;
; (run-tests)
