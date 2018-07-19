(ns repl-tooling.eval-test
  (:require [repl-tooling.eval :as eval]
            [check.core :refer-macros [check]]
            [clojure.test :refer-macros [run-tests]]
            [check.async-cljs :refer-macros [def-async-test await!]]))

; (def-async-test "Tries to deduce ")

(run-tests)
