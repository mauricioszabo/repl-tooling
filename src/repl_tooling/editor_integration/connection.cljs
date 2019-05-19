(ns repl-tooling.editor-integration.connection
  (:require [repl-tooling.repl-client :as repl-client]
            [repl-tooling.editor-helpers :as helpers]
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
  (when-let [out (:out output)] (and on-stdout (on-stdout out)))
  (when-let [out (:err output)] (and on-stderr (on-stderr out)))
  (when (or (:result output) (:error output))
    (and on-result (on-result (helpers/parse-result output)))))

(defn- ensure-data [data-or-promise call]
  (if (instance? js/Promise data-or-promise)
    (. data-or-promise then #(call %))
    (call data-or-promise)))

(defn- eval-cmd [repl code range filename row col namespace on-eval on-start]
  (when code
    (let [id (atom nil)]
      (reset! id (eval/evaluate repl
                                code
                                {:filename filename
                                 :row row
                                 :col col
                                 :namespace (str namespace)}
                                #(and on-eval (on-eval (helpers/parse-result %)
                                                       @id
                                                       range))))
      (and on-start (on-start @id range)))))

(defn- eval-block [repl data on-start-eval on-eval]
  (ensure-data data
               (fn [{:keys [contents range filename] :as data}]
                 (let [[[row col]] range
                       code (helpers/read-next contents (inc row) (inc col))
                       [_ namespace] (helpers/ns-range-for contents [row col])]
                   ;FIXME: It's not this range!
                   (eval-cmd repl code filename row col namespace range
                             on-eval on-start-eval)))))

(defn- eval-top-block [repl data on-start-eval on-eval]
  (ensure-data data
               (fn [{:keys [contents range filename] :as data}]
                 (let [[start] range
                       [eval-range code] (helpers/top-block-for contents start)
                       [[s-row s-col]] eval-range
                       [_ namespace] (helpers/ns-range-for contents [s-row s-col])]
                   (eval-cmd repl code filename s-row s-col namespace eval-range
                             on-eval on-start-eval)))))

(defn- eval-selection [repl data on-start-eval on-eval]
  (ensure-data data
               (fn [{:keys [contents range filename] :as data}]
                 (let [[[row col]] range
                       code (helpers/text-in-range contents range)
                       [_ namespace] (helpers/ns-range-for contents [row col])]
                   (eval-cmd repl code filename row col namespace range
                             on-eval on-start-eval)))))

(defn- cmds-for [aux primary {:keys [editor-data on-start-eval on-eval]}]
  {:evaluate-top-block {:name "Evaluate Top Block"
                        :description "Evaluates top block block on current editor's selection"
                        :command #(eval-top-block primary (editor-data) on-start-eval on-eval)}
   :evaluate-block {:name "Evaluate Block"
                    :description "Evaluates current block on editor's selection"
                    :command #(eval-block primary (editor-data) on-start-eval on-eval)}
   :evaluate-selection {:name "Evaluate Selection"
                        :description "Evaluates current editor's selection"
                        :command #(eval-selection primary (editor-data) on-start-eval on-eval)}
   :break-evaluation {:name "Break Evaluation"
                      :description "Break current running eval"
                      :command #(eval/break primary aux)}
   :disconnect {:name "Disconnect REPLs"
                :description "Disconnect all current connected REPLs"
                :command disconnect!}})

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
  [host port {:keys [on-stdout on-stderr on-result on-disconnect
                     editor-data on-start-eval on-eval] :as opts}]
  (js/Promise.
   (fn [resolve]
     (let [callback (partial callback on-stdout on-stderr on-result on-disconnect)
           aux (clj-repl/repl :clj-aux host port callback)
           primary (delay (clj-repl/repl :clj-eval host port callback))
           state (atom nil)
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
                                (fn []
                                  (reset! state {:clj/aux aux
                                                 :clj/repl @primary
                                                 :editor/commands (cmds-for aux @primary opts)})
                                  (resolve @state))))]

       (eval/evaluate aux ":aux-connected" {:ignore true}
                      #(connect-primary))))))

(defn connect!
  "Connects to a clojure and upgrade to UNREPL protocol. Expects host, port, and three
callbacks:
* on-stdout -> a function that receives a string when some code prints to stdout
* on-stderr -> a function that receives a string when some code prints to stderr
* on-result -> returns a clojure EDN with the result of code
* on-disconnect -> called with no arguments, will disconnect REPLs. Can be called more
than once

Returns a promise that will resolve to a map with two repls: :clj/aux will be used
to autocomplete/etc, :clj/repl will be used to evaluate code."
  [host port {:keys [on-stdout on-stderr on-result on-disconnect
                     editor-data on-start-eval on-eval cljs?] :as opts}]
  (connect-unrepl! host port opts))
