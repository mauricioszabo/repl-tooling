(ns repl-tooling.editor-integration.connection
  (:require [reagent.core :as r]
            [promesa.core :as p]
            [clojure.string :as str]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.editor-integration.autocomplete :as autocomplete]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.editor-integration.renderer :as renderer]
            [repl-tooling.editor-integration.schemas :as schemas]
            [repl-tooling.repl-client.nrepl :as nrepl]
            [repl-tooling.commands-to-repl.all-cmds :as cmds]
            [repl-tooling.commands-to-repl.pathom :as pathom]
            [repl-tooling.editor-integration.commands :as commands]
            [schema.core :as s]
            [repl-tooling.editor-integration.definition :as definition]
            [repl-tooling.editor-integration.configs :as configs]
            ["fs" :refer [exists readFile existsSync]]
            ["path" :refer [join]]))

; FIXME: This only here because of tests
(defn disconnect!
  "Disconnect all REPLs. Indempotent."
  []
  (repls/disconnect! :clj-eval)
  (repls/disconnect! :clj-aux)
  (repls/disconnect! :cljs-aux)
  (repls/disconnect! :cljs-eval))

(defn- features-for [state {:keys [editor-data] :as opts} _repl-kind]
  {:autocomplete #(p/let [data (editor-data)]
                    (autocomplete/command state opts data))
   ; TODO: Deprecate this
   :eval-and-render (fn eval-and-render
                      ([code range] (eval-and-render code range nil))
                      ([code range pass]
                       (p/let [data (editor-data)]
                         (cmds/eval-range state
                                          data
                                          (assoc opts :pass pass)
                                          (constantly [range code])))))
   :evaluate-and-render (fn [options]
                          (p/let [data (editor-data)
                                  {:keys [text range]} options]
                            (cmds/eval-range state
                                             data
                                             (dissoc options :text :range)
                                             (constantly [range text]))))
   :eval (fn [options]
           (let [code (:text options)
                 [[row col]] (:range options)
                 eval-opts (cond-> (dissoc options :text)
                                   row (assoc :row row)
                                   col (assoc :col col))]
             (e-eval/eval-with-promise state code eval-opts)))
   :result-for-renderer #(renderer/parse-result (:result %) (:repl %) state)
   :go-to-var-definition #(definition/goto-definition state %)
   :get-full-var-name #(cmds/fqn-for-var state)
   :get-code #(e-eval/get-code state %)
   :repl-for #(e-eval/repl-for state %1 %2)
   :eql (pathom/eql-from-state state)})

(defn- file-exists? [file]
  (js/Promise. (fn [resolve] (exists file resolve))))

(defn- read-file [editor-state file]
  (let [run-callback (:run-callback @editor-state)
        existing-file (->> (run-callback :get-config)
                           :project-paths
                           (cons ".")
                           (map #(join % file))
                           (filter #(existsSync %))
                           first)]
    (js/Promise. (fn [resolve]
                   (if existing-file
                     (readFile existing-file (fn [error not-error]
                                               (if error
                                                 (resolve nil)
                                                 (resolve (str not-error)))))
                     (resolve nil))))))

(def ^:private default-opts
  {:on-start-eval identity
   :file-exists file-exists?
   :config-file-path nil
   :register-commands identity
   :open-editor identity
   :get-rendered-results (constantly [])
   :on-copy identity
   :on-eval identity
   :on-result identity
   :on-stdout identity
   :on-stderr identity
   :editor-data identity
   :notify identity
   :get-config (constantly {:project-paths [], :eval-mode :prefer-clj})
   :prompt (fn [ & _] (js/Promise. (fn [])))})

(defn- swap-state! [state options kind]
  (p/let [cmds (cmds/all state options kind)
          feats (features-for state options kind)]
    (swap! state assoc
           :editor/features feats
           :run-callback (partial commands/run-callback! state)
           :run-feature (partial commands/run-feature! state))
    (swap! state update-in [:editor/callbacks :read-file]
           #(or % (partial read-file state)))
    (configs/prepare-commands state cmds)))

(defn connect-evaluator!
  ""
  [evaluators opts]
  (js/Promise.
   (fn [resolve]
     (let [state (atom evaluators)
           options (merge default-opts opts)]

       ; TODO: Check this last parameter
       (swap-state! state options :clj)
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
                      :repl/info {:host host :port port :kind :cljs :kind-name (tr-kind :cljs)}})
  (eval/eval primary "(set! lumo.repl/*pprint-results* false)" {:ignore true})
  (swap-state! state options :cljs))

(defn- prepare-joker [primary host port state options]
  (swap! state merge {:clj/repl primary
                      :clj/aux primary
                      :repl/info {:host host :port port
                                  :kind :joker :kind-name (tr-kind :joker)}})
  (swap-state! state options :joker))

(defn- prepare-generic [primary aux host port state options kind]
  (when (= :clj kind)
    ; (clj-repl/disable-limits! primary)
    (eval/evaluate aux ":aux-connected" {:ignore true} #(clj-repl/disable-limits! aux)))

  (swap! state merge {:clj/aux aux
                      :clj/repl primary
                      :repl/info {:host host :port port :kind kind :kind-name (tr-kind kind)}})
  (swap-state! state options kind))

(defn- connection-error! [error notify]
  (disconnect!)
  (if (= "ECONNREFUSED" error)
    (notify {:type :error
             :title "REPL not connected"
             :message (str "Connection refused. Ensure that you have a "
                           "Socket REPL started on this host/port")})
    (do
      (notify {:type :error
               :title "REPL not connected"
               :message (str "Unknown error while connecting to the REPL: "
                             error)})
      (.error js/console error)))
  nil)

(defn- callback-fn [state output]
  (let [{:keys [on-stdout on-stderr on-result on-disconnect on-patch]}
        (:editor/callbacks @state)]
    (when (and (nil? output) on-disconnect)
      (cmds/handle-disconnect! state)
      (on-disconnect))
    (when-let [out (:out output)] (and on-stdout (on-stdout out)))
    (when-let [out (:err output)] (and on-stderr (on-stderr out)))
    (when (and on-result (or (contains? output :result)
                             (contains? output :error)))
      (on-result (helpers/parse-result output)))
    (when-let [patch (:patch output)]
      (on-patch (update patch :result helpers/parse-result)))))

(defn- find-patch [id maybe-coll]
  (let [elem (if (instance? reagent.ratom/RAtom maybe-coll)
               (dissoc @maybe-coll :editor-state :repl)
               maybe-coll)]
    (if (and (instance? renderer/Patchable elem)
             (= id (:id elem)))
      maybe-coll
      (when (coll? elem)
        (->> elem
             (map #(find-patch id %))
             flatten
             (filter identity))))))

(defn- prepare-patch [{:keys [on-patch get-rendered-results] :as callbacks}]
  (if on-patch
    callbacks
    (assoc callbacks
           :on-patch (fn [{:keys [id result]}]
                       (doseq [patchable (find-patch id (get-rendered-results))]
                         (swap! patchable assoc :value
                                (renderer/parse-result result
                                                       (:repl @patchable)
                                                       (:editor-state @patchable))))))))

(defn- callback-aux [original-callback]
  (fn [msg]
    (if (or (:out msg) (:err msg))
      (when helpers/*out-on-aux* (original-callback msg))
      (original-callback msg))))

; Config Options:
; {:project-paths [...]
;  :eval-mode (enum :clj :cljs :prefer-clj :prefer-cljs)}
(s/defn connect!
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
* open-editor -> asks the editor to open an editor. Expects a map with `:filename`,
  `:line` and maybe `:contents`. If there's `:contents` key, it defines a \"virtual
  file\" so it's better to open up an read-only editor
* notify -> when something needs to be notified, this function will be called with a map
  containing :type (one of :info, :warning, or :error), :title and :message
* get-config -> when some function needs the configuration from the editor, this fn
  is called without arguments. Need to return a map with the config options.
* get-rendered-results -> gets all results that are rendered on the editor. This is
  used so that the REPL can 'patch' these results when new data appears (think
  of resolving promises in JS)
* on-patch -> patches the result. Optional, if you send a :get-rendered-results
  callback, one will be generated for you
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
   {:keys [notify] :as opts} :- s/Any]
  (p/catch
   (p/let [options (-> default-opts (merge opts) prepare-patch)
           state (atom {:editor/callbacks options})
           callback (partial callback-fn state)
           [kind primary] (repls/connect-repl! :clj-eval host port callback)
           _ (eval/eval primary "1234")
           _ (case kind
               :cljs (prepare-cljs primary host port state options)
               :joker (prepare-joker primary host port state options)
               (p/let [[_ aux] (repls/connect-repl! :clj-aux host port (callback-aux callback))]
                 (prepare-generic primary aux host port state options kind)))
           nrepl? (instance? nrepl/Evaluator primary)]
     (do
       (notify {:type :info :title (str (tr-kind kind)
                                        (if nrepl? " nREPL" " socket REPL")
                                        " Connected")})
       state))
   #(connection-error! % notify)))

(defn connect-callbacks!
  "Connects callbacks only, for commands that can work without a REPL."
  [callbacks]
  (let [options (merge default-opts callbacks)
        state-ish (atom  {:editor/callbacks options})
        callback-cmds (commands/->Callbacks state-ish)]
    (swap! state-ish assoc
           :editor/features {:result-for-renderer
                             #(renderer/parse-result (:result %)
                                                     (:repl %)
                                                     state-ish)
                             ;; FIXME: Re-add pathom without REPL
                             :eql (constantly nil)} ;(partial pathom/eql {:callbacks options})}
           :run-callback (partial commands/run-callback! callback-cmds)
           :run-feature (partial commands/run-feature! callback-cmds))
    ((:register-commands options) (cmds/static-commands state-ish))
    state-ish))
