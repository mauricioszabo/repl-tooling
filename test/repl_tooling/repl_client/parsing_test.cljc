(ns repl-tooling.repl-client.parsing-test
  (:require [clojure.test :refer [async testing is] :include-macros true]
            [check.core :refer-macros [check]]
            [clojure.core.async :as async :include-macros true]
            [repl-tooling.repl-client :as client]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval-helpers :refer-macros [eval-on-repl eval-and-parse]]
            [repl-tooling.repl-client.clojure :as clj]))

(set! cards/test-timeout 8000)
(cards/deftest evaluate-ellisions
  (async done
    (async/go
     (client/disconnect! :clj-ellisions-1)
     (let [repl (clj/repl :clj-ellisions-1 "localhost" 2233 identity)]
       (testing "ellisions on lists"
         (let [res (eval-and-parse "(range)")
               ellided (async/promise-chan)
               ellide-fn (eval/get-more-fn (:result res))]
           (check (:as-text res) => "(0 1 2 3 4 5 6 7 8 9 ...)")
           (check (eval/without-ellision (:result res)) => '(0 1 2 3 4 5 6 7 8 9))
           (ellide-fn repl #(async/put! ellided %))
           (check (eval/without-ellision (async/<! ellided)) =>
                  '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))))

       (client/disconnect! :clj-ellisions-1)
       (done)))))
