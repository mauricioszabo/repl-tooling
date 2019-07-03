(ns repl-tooling.editor-integration.connection
  (:require [repl-tooling.repl-client :as repl-client]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.editor-integration.loaders :as loaders]
            [repl-tooling.editor-integration.evaluation :as e-eval]))

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

(defn- eval-block [state data opts]
  (ensure-data data
               (fn [{:keys [contents range] :as data}]
                 (let [[[row col]] range
                       code (helpers/read-next contents (inc row) (inc col))
                       [_ namespace] (helpers/ns-range-for contents [row col])]
                   ;FIXME: It's not this range!
                   (e-eval/eval-cmd state code namespace range data opts)))))

(defn- eval-top-block [state data opts]
  (ensure-data data
               (fn [{:keys [contents range] :as data}]
                 (let [[start] range
                       [eval-range code] (helpers/top-block-for contents start)
                       [[s-row s-col]] eval-range
                       [_ namespace] (helpers/ns-range-for contents [s-row s-col])]
                   (e-eval/eval-cmd state code namespace eval-range data opts)))))

(defn- eval-selection [state data opts]
  (ensure-data data
               (fn [{:keys [contents range] :as data}]
                 (let [[[row col]] range
                       code (helpers/text-in-range contents range)
                       [_ namespace] (helpers/ns-range-for contents [row col])]
                   (e-eval/eval-cmd state code namespace range data opts)))))

(defn- cmds-for [state {:keys [editor-data] :as opts}]
  (let [primary (:clj/repl @state)
        aux (:clj/aux @state)]
    {:evaluate-top-block {:name "Evaluate Top Block"
                          :description "Evaluates top block block on current editor's selection"
                          :command #(eval-top-block state (editor-data) opts)}
     :evaluate-block {:name "Evaluate Block"
                      :description "Evaluates current block on editor's selection"
                      :command #(eval-block state (editor-data) opts)}
     :evaluate-selection {:name "Evaluate Selection"
                          :description "Evaluates current editor's selection"
                          :command #(eval-selection state (editor-data) opts)}
     :break-evaluation {:name "Break Evaluation"
                        :description "Break current running eval"
                        :command #(eval/break primary aux)}
     :load-file {:name "Load File"
                 :description "Loads current file on a Clojure REPL"
                 :command (fn [] (ensure-data (editor-data)
                                              #(loaders/load-file opts aux %)))}
     :disconnect {:name "Disconnect REPLs"
                  :description "Disconnect all current connected REPLs"
                  :command disconnect!}}))

(defn- disable-limits! [aux]
  (eval/evaluate aux
                 (clj-repl/unrepl-cmd (-> aux :session deref :state)
                                      :print-limits
                                      {:unrepl.print/string-length 9223372036854775807
                                       :unrepl.print/coll-length 9223372036854775807
                                       :unrepl.print/nesting-depth 9223372036854775807})
                 {:ignore true}
                 identity))

(defn connect-unrepl!
  "Connects to a clojure and upgrade to UNREPL protocol. Expects host, port, and three
callbacks:
* on-start-eval -> a function that'll be called when an evaluation starts
* on-eval -> a function that'll be called when an evaluation ends
* editor-data -> a function that'll be called when a command needs editor's data.
  Editor's data is a map (or a promise that resolves to a map) with the arguments:
    :contents - the editor's contents.
    :filename - the current file's name. Can be nil if file was not saved yet.
    :range - a vector containing [[start-row start-col] [end-row end-col]], representing
      the current selection
* notify -> when something needs to be notified, this function will be called with a map
  containing :type (one of :info, :warning, or :error), :title and :message
* get-config -> when some function needs the configuration from the editor, this fn
  is called without arguments. Need to return a map with the config options.
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
                             (disable-limits! aux)
                             (eval/evaluate @primary ":primary-connected" {:ignore true}
                                (fn []
                                  (reset! state {:clj/aux aux
                                                 :clj/repl @primary
                                                 :editor/commands (cmds-for state opts)})
                                  (resolve state))))]

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
