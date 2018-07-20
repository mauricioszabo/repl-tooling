(ns repl-tooling.repl-client.protocols-test
  (:require [clojure.test :refer-macros [deftest testing is run-tests]]
            [check.core :refer-macros [check]]
            [check.async-cljs :refer-macros [def-async-test await!]]
            [cljs.core.async :refer [>!] :refer-macros [go]]
            [repl-tooling.repl-client.protocols :as repl]))

(def s1 (atom nil))
(def-async-test "Tests if we only capture the output for last command"
  {:teardown (some-> @s1 .end)}
  (let [[in out socket] (repl/connect-socket! "localhost" 5550)]
    (reset! s1 socket)
    (while (not= (await! out) ""))
    (testing "single line"
      (go (>! in "(+ 1 2 3)"))
      (check (await! out) => "6"))

    (testing "multiple lines"
      (go (>! in "(+\n1\n2\n3)\n"))
      (check (await! out) => "6"))))

(run-tests)
