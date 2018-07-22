(ns ^:figwheel-always repl-tooling.all-tests
  (:require [cljs.nodejs :as nodejs]
            [cljs.test]
            [repl-tooling.repl-client.protocols-test]
            [repl-tooling.repl-client-test]
            [repl-tooling.eval-test]
            [repl-tooling.features.base-test]))

(nodejs/enable-util-print!)

(defn -main [])

(set! *main-cli-fn* -main)

(def process (js/require "process"))
(defmethod cljs.test/report [:cljs.test/default :end-run-tests] [m]
  (when-not (cljs.test/successful? m)
    (println "Some tests failed")
    (aset process "exitCode" 1)))
