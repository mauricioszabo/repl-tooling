(ns repl-tooling.eval-test
  (:require [clojure.test :refer-macros [testing run-tests]]
            [check.core :refer-macros [check]]
            [check.async-cljs :refer-macros [def-async-test await!]]
            [cljs.core.async :refer [>! chan] :refer-macros [go]]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client :as client]
            [repl-tooling.repl-client.lumo :as lumo]))

; (def-async-test "Simulates a request-response"
;   {:teardown (client/disconnect! :lumo-eval)}
;   (let [[in out] (lumo/connect-socket! :lumo-eval "localhost" 5550)
;         result (chan)
;         output (chan)
;         evaluator (eval/evaluator in out #(go (>! output %)))]
;     (eval/eval evaluator '(do (println "FOO") (+ 1 2)) #(go (>! result %)))
;     (check (await! result) => "3")
;     (check (await! output) => "FOO\n")))
;
; (run-tests)
