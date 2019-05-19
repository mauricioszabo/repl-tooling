(ns repl-tooling.repl-client.evaluation-test
  (:require [clojure.test :refer-macros [testing async is]]
            [matcher-combinators.test]
            [devcards.core :as cards :include-macros true]
            [check.core :refer-macros [check]]
            [clojure.core.async :as async :include-macros true]
            [repl-tooling.repl-client :as client]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj])
  (:require-macros [repl-tooling.eval-helpers :refer [eval-on-repl]]))

(set! cards/test-timeout 8000)
(cards/deftest clojure-evaluation
  (async done
    (client/disconnect! :evaluation-test)
    (async/go
     (let [out (async/chan)
           repl (clj/repl :evaluation-test "localhost" 2233 #(some->> % (async/put! out)))]
       #_
       (testing "evaluating request-response"
         (is (= {:result "3" :as-text "3"} (eval-on-repl "(+ 1 2)")))
         (is (= {:result "3" :as-text "3"} (async/<! out))))

       #_
       (testing "capturing output"
         (is (= {:result "nil" :as-text "nil"} (eval-on-repl "(println :foobar)")))
         (is (= {:out ":foobar\n"} (async/<! out)))
         (is (= {:result "nil" :as-text "nil"} (async/<! out))))

       #_
       (testing "passing args to result"
         (let [res (async/promise-chan)]
           (eval/evaluate repl "(+ 2 3)" {:pass {:literal true}} #(async/put! res %))
           (is (= {:as-text "5" :result "5" :literal true} (async/<! res)))
           (is (= {:as-text "5" :result "5" :literal true} (async/<! out)))))

       #_
       (testing "passing parameters to evaluation"
         (let [res (async/promise-chan)]
           (eval/evaluate repl "(/ 10 0)" {:filename "foo.clj" :row 12 :col 0}
                          #(async/put! res %))
           (is (re-find #"foo\.clj\" 12" (-> res async/<! :error)))))

       (testing "bust evaluations"
         (let [res (async/chan)]
           (doseq [n (range 5)]
             (prn :EVAL n)
             (eval/evaluate repl (str ":foo" n) {} #(do
                                                      (prn :RES %)
                                                      (async/put! res %))))
           (is (= ":foo0" (-> res async/<! :result)))
           (is (= ":foo1" (-> res async/<! :result)))
           (is (= ":foo2" (-> res async/<! :result)))
           (is (= ":foo3" (-> res async/<! :result)))
           (is (= ":foo4" (-> res async/<! :result)))
           (async/<! (async/timeout 1000))))

      (client/disconnect! :evaluation-test)
      (done)))))
