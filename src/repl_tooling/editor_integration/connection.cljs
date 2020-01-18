(ns repl-tooling.editor-integration.connection
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.editor-integration.loaders :as loaders]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.editor-integration.embedded-clojurescript :as embedded]
            [repl-tooling.editor-integration.autocomplete :as autocomplete]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.editor-integration.renderer :as renderer]
            [repl-tooling.editor-integration.doc :as doc]
            [repl-tooling.editor-integration.schemas :as schemas]
            [schema.core :as s]
            [paprika.schemas :as schema :include-macros true]))

(defn disconnect!
  "Disconnect all REPLs. Indempotent."
  []
  (repls/disconnect! :clj-eval)
  (repls/disconnect! :clj-aux)
  (repls/disconnect! :cljs-aux)
  (repls/disconnect! :cljs-eval))

(defn- handle-disconnect!
  "Disconnect all REPLs. Indempotent."
  [state]
  (disconnect!)
  (reset! state nil))

(defn- ensure-data [data-or-promise call]
  (if (instance? js/Promise data-or-promise)
    (. data-or-promise then #(call %))
    (call data-or-promise)))

(defn- eval-range [state {:keys [contents range] :as data} opts function]
  (let [[start] range
        [eval-range code] (function contents start)
        [_ namespace] (helpers/ns-range-for contents (first eval-range))]
    (e-eval/eval-cmd state code namespace eval-range data opts)))

(defn- eval-block [state data opts]
  (ensure-data data #(eval-range state % opts helpers/block-for)))

(defn- eval-top-block [state data opts]
  (ensure-data data #(eval-range state % opts helpers/top-block-for)))

(defn- eval-selection [state data opts]
  (ensure-data data
    (fn [{:keys [range] :as data}]
      (eval-range state data opts
                  (fn [contents _]
                    [range (helpers/text-in-range contents range)])))))

(defn- cmds-for [state {:keys [editor-data] :as opts} repl-kind]
  (cond->
   {:evaluate-top-block {:name "Evaluate Top Block"
                         :description "Evaluates top block block on current editor's selection"
                         :command #(eval-top-block state (editor-data) opts)}
    :evaluate-block {:name "Evaluate Block"
                     :description "Evaluates current block on editor's selection"
                     :command #(eval-block state (editor-data) opts)}
    :evaluate-selection {:name "Evaluate Selection"
                         :description "Evaluates current editor's selection"
                         :command #(eval-selection state (editor-data) opts)}
    :disconnect {:name "Disconnect REPLs"
                 :description "Disconnect all current connected REPLs"
                 :command #(handle-disconnect! state)}
    :doc-for-var {:name "Documentation for current var"
                  :description "Shows documentation for the current var under cursor"
                  :command (fn [] (ensure-data (editor-data)
                                               #(doc/doc-for-var % opts state)))}
    ; :spec-for-var {:name "Spec for current var"
    ;                :description "Shows spec for the current var under cursor if it exists"
    ;                :command (fn [] (ensure-data (editor-data)
    ;                                             #(doc/specs-for-var % opts state)))}
    :load-file {:name "Load File"
                :description "Loads current file on a Clojure REPL"
                :command (fn [] (ensure-data (editor-data)
                                             #(loaders/load-file
                                               opts {:repl-kind (-> @state :repl/info :kind)
                                                     :repl-name (-> @state :repl/info :kind-name)
                                                     :repl (:clj/aux @state)
                                                     :editor-data %})))}}

   (= :clj repl-kind)
   (assoc
    :break-evaluation {:name "Break Evaluation"
                       :description "Break current running eval"
                       :command #(eval/break (:clj/repl @state) (:clj/aux @state))}
    :connect-embedded {:name "Connect Embedded ClojureScript REPL"
                       :description "Connects to a ClojureScript REPL inside a Clojure one"
                       :command #(embedded/connect! state opts)})))

(s/defn result-for-renderer
  [res :- schemas/EvalResult,
   state
   {:keys [filename]} :- {:filename s/Str, s/Any s/Any}
   {:keys [get-config]}]
  (let [repl (if (e-eval/need-cljs? (get-config) filename)
               (:cljs/repl @state)
               (:clj/repl @state))]
    (renderer/parse-result (:result res) repl state)))

(defn- features-for [state {:keys [editor-data] :as opts} repl-kind]
  {:autocomplete (if (= :bb repl-kind)
                   (constantly (. js/Promise resolve []))
                   #(ensure-data (editor-data)
                                (fn [data] (autocomplete/command state opts data))))
   :eval-and-render (fn eval-and-render
                      ([code range] (eval-and-render code range nil))
                      ([code range pass]
                       (ensure-data (editor-data)
                                    #(eval-range state
                                                 %
                                                 (assoc opts :pass pass)
                                                 (constantly [range code])))))
   :eval (partial e-eval/eval-with-promise state opts)
   :result-for-renderer #(ensure-data (editor-data)
                                      (fn [data] (result-for-renderer % state data opts)))})

(def ^:private default-opts
  {:on-start-eval identity
   :on-eval identity
   :editor-data identity
   :notify identity
   :get-config identity ;FIXME
   :prompt (fn [ & _] (js/Promise. (fn [])))})

(defn connect-evaluator!
  ""
  [evaluators opts]
  (js/Promise.
   (fn [resolve]
     (let [state (atom evaluators)
           options (merge default-opts opts)]

       ; TODO: Check this last parameter
       (swap! state assoc
              :editor/commands (cmds-for state options :clj)
              :editor/features (features-for state options :clj))
       (resolve state)))))

(defn- tr-kind [kind]
  (let [kinds {:clj "Clojure"
               :cljs "ClojureScript"
               :cljr "ClojureCLR"
               :clje "Clojerl"
               :bb "Babaska"}]
    (kinds kind (-> kind name str/capitalize))))

(defn- prepare-cljs [primary host port state options]
  (swap! state merge {:cljs/repl primary
                      :repl/info {:host host :port port :kind :cljs :kind-name (tr-kind :cljs)}
                      :editor/commands (cmds-for state options :cljs)
                      :editor/features (features-for state options :cljs)}))

(defn- prepare-joker [primary host port state options]
  (swap! state merge {:clj/repl primary
                      :clj/aux primary
                      :repl/info {:host host :port port
                                  :kind :joker :kind-name (tr-kind :joker)}
                      :editor/commands (cmds-for state options :joker)
                      :editor/features (features-for state options :joker)}))

(defn- prepare-generic [primary aux host port state options kind]
  (when (= :clj kind)
    (eval/evaluate aux ":aux-connected" {:ignore true} #(clj-repl/disable-limits! aux)))

  (swap! state merge {:clj/aux aux
                      :clj/repl primary
                      :repl/info {:host host :port port :kind kind :kind-name (tr-kind kind)}
                      :editor/commands (cmds-for state options kind)
                      :editor/features (features-for state options kind)}))

(defn- connection-error! [error notify]
  (if (= "ECONNREFUSED" error)
    (notify {:type :error
             :title "REPL not connected"
             :message (str "Connection refused. Ensure that you have a "
                           "Socket REPL started on this host/port")})
    (notify {:type :error
             :title "REPL not connected"
             :message (str "Unknow error while connecting to the REPL: "
                           error)}))
  nil)

(defn- callback-fn [state on-stdout on-stderr on-result on-disconnect output]
  (when (nil? output)
    (handle-disconnect! state)
    (and on-disconnect (on-disconnect)))
  (when-let [out (:out output)] (and on-stdout (on-stdout out)))
  (when-let [out (:err output)] (and on-stderr (on-stderr out)))
  (when (and on-result (or (contains? output :result)
                           (contains? output :error)))
    (on-result (helpers/parse-result output))))

; Config Options:
; {:project-paths [...]
;  :eval-mode (enum :clj :cljs :prefer-clj :prefer-cljs)}
(schema/defn-s connect!
  "Connects to a clojure-like REPL that supports the socket REPL protocol.
Expects host, port, and some callbacks:
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
* prompt -> when some function needs an answer from the editor, it'll call this
  callback passing :title, :message, and :arguments (a vector that is composed by
  :key and :value). The callback needs to return a `Promise` with one of the
  :key from the :arguments, or nil if nothing was selected.
* on-copy -> a function that receives a string and copies its contents to clipboard
* on-stdout -> a function that receives a string when some code prints to stdout
* on-stderr -> a function that receives a string when some code prints to stderr
* on-result -> returns a clojure EDN with the result of code
* on-disconnect -> called with no arguments, will disconnect REPLs. Can be called more
than once

Returns a promise that will resolve to a map with two repls: :clj/aux will be used
to autocomplete/etc, :clj/repl will be used to evaluate code."
  [host :- s/Str
   port :- s/Int
   {:keys [on-stdout on-stderr on-result on-disconnect notify] :as opts} :- s/Any]
  (let [options (merge default-opts opts)
        state (r/atom {:editor/callbacks options})
        callback (partial callback-fn state on-stdout on-stderr on-result on-disconnect)
        primary (repls/connect-repl! :clj-eval host port callback)
        aux (delay (repls/connect-repl! :clj-aux host port callback))]

    (.. primary
        (then (fn [[kind primary]]
                (notify {:type :info
                         :title (str (tr-kind kind) " REPL Connected")})
                (.. js/Promise
                    (resolve
                     (case kind
                       :cljs (prepare-cljs primary host port state options)
                       :joker (prepare-joker primary host port state options)
                       (.then @aux (fn [[_ aux]]
                                     (prepare-generic primary aux host port state
                                                      options kind)))))
                    (then (fn [] state)))))
        (catch #(connection-error! % notify)))))
