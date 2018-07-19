(ns repl-tooling.repl-client-test
  (:require [clojure.test :refer-macros [deftest testing is run-tests]]
            [check.core :refer-macros [check]]
            [check.async-cljs :refer-macros [def-async-test await!]]
            [cljs.core.async :refer [>!] :refer-macros [go]]
            [repl-tooling.repl-client :as client]
            [repl-tooling.repl-client.generic :as generic]
            [repl-tooling.repl-client.lumo :as lumo]))

; (def-async-test "Connecting to some generic socket REPL"
;   {:teardown (client/disconnect! :generic)}
;   (let [[in out] (generic/connect-socket! :generic "localhost" 5550)]
;     (check (:out (await! out)) => #"Lumo")
;
;     (testing "sending commands"
;       (go (>! in '(+ 1 2)))
;       (check (await! out) => {:out "" :result "3"})
;       (go (>! in '(+ 1 6)))
;       (check (await! out) => {:out "" :result "7"}))
;
;     (testing "send composite commands"
;       (go (>! in '(do (println "Foo") (+ 2 3))))
;       (check (await! out) => {:out "Foo" :result "5"}))))

(def-async-test "Connecting to socket lumo REPL" {:teardown (client/disconnect! :lumo)}
  (let [[in out] (lumo/connect-socket! :lumo "localhost" 5550)]
    (testing "sending commands"
      (check (:out (await! out)) => #"Lumo")
      (go (>! in '(+ 1 2)))
      (check (await! out) => {:out "" :result "3"}))))
      ; (go (>! in '(+ 1 6)))
      ; (check (await! out) => {:out "" :result "7"}))))

(run-tests)
