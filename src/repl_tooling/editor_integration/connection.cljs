(ns repl-tooling.editor-integration.connection
  (:require [repl-tooling.repl-client :as repl-client]
            [repl-tooling.editor-helpers :as editor-helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj-repl]))

(defn disconnect!
  "Disconnect all REPLs. Indempotent."
  []
  (repl-client/disconnect! :clj-eval)
  (repl-client/disconnect! :clj-aux)
  (repl-client/disconnect! :cljs-eval))

(defn- callback [on-stdout on-stderr on-result on-disconnect output]
  (when (nil? output)
    (disconnect!)
    (on-disconnect))
  (when-let [out (:out output)] (on-stdout out))
  (when-let [out (:err output)] (on-stderr out))
  (when (or (:result output) (:error output))
    (on-result (editor-helpers/parse-result output))))

(defn connect-unrepl!
  "Connects to a clojure and upgrade to UNREPL protocol. Expects host, port, and three
callbacks:
* on-stdout -> a function that receives a string when some code prints to stdout
* on-stderr -> a function that receives a string when some code prints to stderr
* on-result -> returns a clojure EDN with the result of code
* on-disconnect -> called with no arguments, will disconnect REPLs. Can be called more
than once

Returns a promise that will resolve to a map with two repls: :clj/aux will be used
to autocomplete/etc, :clj/repl will be used to evaluate code."
  [host port on-stdout on-stderr on-result on-disconnect]
  (js/Promise.
   (fn [resolve]
     (let [callback (partial callback on-stdout on-stderr on-result on-disconnect)
           aux (clj-repl/repl :clj-aux host port callback)
           primary (delay (clj-repl/repl :clj-eval host port callback))
           connect-primary (fn []
                             (eval/evaluate aux
                                            (clj-repl/unrepl-cmd (-> aux :session deref :state)
                                                                 :print-limits
                                                                 {:unrepl.print/string-length 9223372036854775807
                                                                  :unrepl.print/coll-length 9223372036854775807
                                                                  :unrepl.print/nesting-depth 9223372036854775807})
                                            {:ignore true}
                                            identity)

                             (eval/evaluate @primary ":primary-connected" {:ignore true}
                                            (fn [] (resolve {:clj/aux aux
                                                             :clj/repl @primary}))))]

       (eval/evaluate aux ":aux-connected" {:ignore true}
                      #(connect-primary))))))
