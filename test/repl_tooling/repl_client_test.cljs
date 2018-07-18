(ns repl-tooling.repl-client-test
  (:require [clojure.test :refer-macros [deftest testing is run-tests]]
            [check.core :refer-macros [check]]
            [check.async-cljs :refer-macros [def-async-test await!]]
            [cljs.core.async :refer [>!] :refer-macros [go]]
            [repl-tooling.repl-client :as client]))

(def-async-test "Connecting to socket lumo REPL" {:teardown (client/disconnect! :lumo)}
  (let [[in out] (client/connect-socket! :lumo "localhost" 5550)]
    (check (:out (await! out)) => #"Lumo")
    (go (>! in '(+ 1 2)))
    (check (await! out) => {:out "3"})
    (go (>! in '(+ 1 6)))
    (check (await! out) => {:out "7"})))

(run-tests)
  ; (:require [repl-tooling.repl-client :as client]))
            ; [check.core :refer-macros [check]]))
  ;           [cljs.core.async :refer [chan >! timeout <!]])
  ; (:require-macros [cljs.core.async.macros :refer [go]]
  ;                  [check.async-cljs :refer-macros [def-async-test]]))

; (println "BAR")
; (def-async-test "SomeTest" {}
;   (let [c (chan)]
;     (go
;      (println "FOO" c)
;      (>! c "Foo")
;      (println "Done?")
;      (println "RES" (<! c)))))
