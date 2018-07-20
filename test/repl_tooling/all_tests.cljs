(ns ^:figwheel-always repl-tooling.all-tests
  (:require [cljs.nodejs :as nodejs]
            [clojure.test :refer-macros [run-tests]]
            [repl-tooling.repl-client.protocols-test]
            [repl-tooling.repl-client-test]
            [repl-tooling.eval-test]))

(nodejs/enable-util-print!)

(defn -main []
  (println "LOADED")
  (run-tests))

(set! *main-cli-fn* -main)


; js/__filename
; (. js/process -env -PWD)
; (. js/process cwd)
; js/__dirname
; (def fs (js/require "fs"))
; (aset js/global "__dirname" (. fs realpathSync "."))
; (. js/global -__dirname)
; (println "FOO")
;
; (run-tests)
