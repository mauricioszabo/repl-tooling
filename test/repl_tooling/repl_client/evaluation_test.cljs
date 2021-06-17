(ns repl-tooling.repl-client.evaluation-test
  (:require [clojure.test :refer [testing async is]]
            [devcards.core :as cards :include-macros true]
            [check.core :refer [check]]
            [clojure.core.async :as async]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj]
            [repl-tooling.eval-helpers :refer [eval-on-repl
                                               async-with-cljs-repl
                                               async-with-clj-repl]]))

(set! cards/test-timeout 20000)
(cards/deftest clojure-evaluation
  (async-with-clj-repl "evaluation"
    (testing "evaluating request-response"
      (is (= {:result "3" :as-text "3"} (eval-on-repl "(+ 1 2)")))
      (is (= {:result "3" :as-text "3"} (async/<! out))))

    (testing "evaluating exceptions"
      (let [res (eval-on-repl "(throw (ex-info \"SomeError\" {}))")]
        (check res
           => {:error #":type clojure.lang.ExceptionInfo.*:message \"SomeError\""}))
      (async/<! out))

    (testing "capturing output"
      (is (= {:result "nil" :as-text "nil"} (eval-on-repl "(println :foobar)")))
      (is (= {:out ":foobar\n"} (async/<! out)))
      (is (= {:result "nil" :as-text "nil"} (async/<! out))))

    (testing "passing args to result"
      (let [res (async/promise-chan)]
        (eval/evaluate repl "(+ 2 3)" {:pass {:literal true}} #(async/put! res %))
        (is (= {:as-text "5" :result "5" :literal true} (async/<! res)))
        (is (= {:as-text "5" :result "5" :literal true} (async/<! out)))
        (async/close! res)))

    (testing "passing parameters to evaluation"
      (let [res (async/promise-chan)]
        (eval/evaluate repl "(/ 10 0)" {:filename "foo.clj" :row 12 :col 0}
                       #(async/put! res %))
        (is (re-find #"foo\.clj\" 12" (-> res async/<! :error)))))

    (testing "canceling an evaluation"
      (let [res (async/promise-chan)]
        (eval/evaluate repl "(Thread/sleep 5000)" {} #(async/put! res %))
        (async/<! (async/timeout 500))
        (eval/break repl aux)
        (is (-> res async/<! :error))))

    (testing "burst evaluations"
      (let [res (async/chan)]
        (doseq [n (range 5)]
          (eval/evaluate repl (str ":foo" n) {} #(async/put! res %)))
        (is (= ":foo0" (-> res async/<! :result)))
        (is (= ":foo1" (-> res async/<! :result)))
        (is (= ":foo2" (-> res async/<! :result)))
        (is (= ":foo3" (-> res async/<! :result)))
        (is (= ":foo4" (-> res async/<! :result)))
        (async/close! res)))

    (testing "burst evaluations with blocks"
      (let [res (async/chan)]
        (doseq [n (range 5)]
          (eval/evaluate repl (str "(str " n ")") {} #(async/put! res %)))
        (is (= "\"0\"" (-> res async/<! :result)))
        (is (= "\"1\"" (-> res async/<! :result)))
        (is (= "\"2\"" (-> res async/<! :result)))
        (is (= "\"3\"" (-> res async/<! :result)))
        (is (= "\"4\"" (-> res async/<! :result)))
        (async/close! res)))))

(cards/deftest clojurescript-evaluation
  (async-with-cljs-repl "evaluation on CLJS"
    (testing "evaluating request-response"
      (is (= {:result "##Inf" :as-text "##Inf"} (eval-on-repl "(/ 10 0)")))
      (is (= {:result "10" :as-text "10"} (eval-on-repl "(+ 5 5)"))))

    (testing "evaluating exceptions"
      (check (eval-on-repl "(throw (ex-info \"SomeError\" {}))")
             => {:error #":type.*cljs.core.ExceptionInfo.*:message \"SomeError\""}))

    (testing "passing args to result"
      (let [res (async/promise-chan)]
        (eval/evaluate repl "(+ 2 3)" {:pass {:literal true}} #(async/put! res %))
        (let [r (async/<! res)]
          (check r => {:as-text "5" :result "5" :literal true}))))

    (testing "sending invalid forms"
      (check (eval-on-repl "(+ 1 2") => {:error string?})
      (check (eval-on-repl "#'async/lol") => {:error string?}))))
