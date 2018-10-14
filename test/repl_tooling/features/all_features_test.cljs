(ns repl-tooling.features.all-features-test
  (:require [clojure.test :refer-macros [testing run-tests]]
            [check.core :refer-macros [check]]
            [check.async-cljs :refer-macros [def-async-test await!]]
            [cljs.core.async :refer [>! put! chan] :refer-macros [go]]
            [repl-tooling.eval :as eval]
            [repl-tooling.features.autocomplete :as complete]
            [repl-tooling.repl-client :as client]
            [repl-tooling.repl-client.lumo :as lumo]))

(def-async-test "Detects a possible autocomplete feature"
  {:teardown (client/disconnect! :lumo-feat)}
  (let [[in out] (lumo/connect-socket! :lumo-feat "localhost" 5550)
        c (chan)
        evaluator (eval/evaluator in out identity)]

    (testing "detects presence of Lumo's autocomplete"
      (complete/detect evaluator #(put! c %))
      (check (keys (await! c)) => [:simple-complete]))

    (testing "autocompletes with Lumo"
      (complete/detect evaluator
                       #(let [complete (:simple-complete %)]
                          (complete evaluator
                                    "clojure.string"
                                    "clojure.string/lowe"
                                    (fn [res]
                                      (put! c res)))))
      (check (await! c) => "[\"clojure.string/lower-case\"]"))))

(run-tests)
