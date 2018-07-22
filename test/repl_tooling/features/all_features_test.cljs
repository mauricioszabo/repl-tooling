(ns repl-tooling.features.all-features-test
  (:require [clojure.test :refer-macros [testing run-tests]]
            [check.core :refer-macros [check]]
            [check.async-cljs :refer-macros [def-async-test await!]]
            [cljs.core.async :refer [>! chan] :refer-macros [go]]
            [repl-tooling.eval :as eval]
            [repl-tooling.features.autocomplete :as complete]
            [repl-tooling.repl-client :as client]
            [repl-tooling.repl-client.lumo :as lumo]))

(def-async-test "Detects a possible autocomplete feature"
  {:teardown (client/disconnect! :lumo-feat)}
  (let [[in out] (lumo/connect-socket! :lumo-feat "localhost" 5550)
        c (chan)
        evaluator (eval/evaluator in out identity)]

    (testing "detects presence of autocomplete"
      (complete/detect evaluator #(go (>! c %)))
      (check (keys (await! c)) => [:simple-complete]))))

(run-tests)

(comment
 (def pair (lumo/connect-socket! :lumo-feat "localhost" 5550))
 (let [[in out] pair
       evaluator (eval/evaluator in out identity)]

   (eval/eval evaluator "lumo.repl/get-completions"
              #(.log js/console "BOO" %)))
 (client/disconnect! :lumo-feat))
  ; (complete/detect evaluator #(go (>! c %)))
  ; (check (keys (await! c)) => [:complete-text]))
