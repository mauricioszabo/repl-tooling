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
  (let [repl (clj/repl :clj-test1 "localhost" 5555 identity)
        out (async/chan)]
    (eval/evaluate repl "(+ 1 2)" {} #(async/put! out %))
    (check (await! out) => "3")))

(run-tests)
