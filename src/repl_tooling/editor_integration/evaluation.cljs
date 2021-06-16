(ns repl-tooling.editor-integration.evaluation
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.schemas :as schemas]
            [repl-tooling.editor-integration.commands :as cmds]
            [schema.core :as s]))

(defn get-code [editor-state kind]
  (p/let [data (cmds/run-callback! editor-state :editor-data)]
    (when-let [{:keys [contents range]} data]
      (when-let [[range text] (case kind
                                :top-block (helpers/top-block-for contents (first range))
                                :block (helpers/block-for contents (first range))
                                :var (helpers/current-var contents (first range))
                                :selection [range (helpers/text-in-range contents range)]
                                :ns (helpers/ns-range-for contents (first range)))]
        {:text text :range range}))))

(defn need-cljs? [config filename]
  (or
   (-> config :eval-mode (= :cljs))
   (and (-> config :eval-mode #{:prefer-clj :prefer-cljs})
        (str/ends-with? (str filename) ".cljs"))
   (and (-> config :eval-mode (= :prefer-cljs))
        (or (str/ends-with? (str filename) ".cljs")
            (str/ends-with? (str filename) ".cljc")
            (str/ends-with? (str filename) ".cljx")))))

(defn- treat-error [notify! cljs? clj-connected?]
  (let [msg (cond
              (and cljs? clj-connected?)
              (str "REPL not connected for ClojureScript.\n\n"
                   "You can connect a REPL using 'Connect Socket REPL' "
                   "command, or 'Connect Embedded' command")

              cljs?
              (str "REPL not connected for ClojureScript.\n\n"
                   "You can connect a REPL using 'Connect Socket REPL' "
                   "command, or by connecting a Clojure REPL and running "
                   "'Connect Embedded' command")

              :else
              (str "REPL not connected for Clojure\n\n"
                   "You can connect a REPL by running 'Connect Socket REPL' command"))]
    (notify! {:type :error
              :title "REPL not connected"
              :message msg})
    nil))

(s/defn repl-for :- s/Any
  [state filename :- (s/maybe s/Str), aux? :- schemas/AuxOptions]
  (let [cljs? (need-cljs? (cmds/run-callback! state :get-config) filename)
        repl (cond
               (and cljs? (not= aux? :always)) (:cljs/repl @state)
               aux? (:clj/aux @state)
               :else (:clj/repl @state))]
    (if (nil? repl)
      (treat-error #(cmds/run-callback! state :notify %) cljs? (:clj/repl @state))
      repl)))

(s/defn eval-cmd [state :- schemas/EditorState
                  code :- s/Str
                  namespace
                  range :- schemas/Range
                  editor-data :- schemas/EditorData
                  opts :- schemas/EvalOpts]
  (when code
    (let [prom (p/deferred)
          filename (:filename editor-data)
          {:keys [on-start-eval on-eval]} (:editor/callbacks @state)
          [[row col]] range
          ;; TODO: Remove UNREPL and always evaluate on primary
          repl (repl-for state filename (-> opts :pass :aux))
          id (gensym)
          eval-data {:id id
                     :editor-data editor-data
                     :range range}]
      (when repl
        (and on-start-eval (on-start-eval eval-data))
        (eval/evaluate repl
                       code
                       (merge {:filename filename
                               :id id
                               :row (inc row)
                               :col (inc col)
                               :namespace namespace}
                              opts)
                       #(when (and on-eval @state)
                          (let [parsed (helpers/parse-result %)]
                            (on-eval (assoc eval-data
                                            :repl repl
                                            :result parsed))
                            (p/resolve! prom (select-keys parsed [:result :error]))))))
      (when-not repl (p/resolve! prom nil))
      prom)))

(defn- auto-opts [editor-data]
  (p/let [{:keys [filename range contents]} (editor-data)
          [[row col]] range
          [_ ns] (helpers/ns-range-for contents (first range))]
    {:filename filename
     :namespace ns
     :row row
     :col col}))

(defn eval-with-promise
  "Evaluates the current code and evaluation options on the current REPL.
Accepts an extra argument on `eval-opts` that's :aux - if true, evaluates
on the 'auxiliary' REPL instead of primary. On Clojure, this means that
the code will use UNREPL but will not use ellisions on infinite sequences, etc.

Please notice that because the REPL is auto-detected, `:filename` is required.
Otherwise, ClojureScript REPL will never be used! You can also pass `:auto-detect true`
to use the current editor state to find all info about current filename, namespace,
and row/col.

Will return a 'promise' that is resolved to the eval result, or failed if the
eval result is an error. It will also return a fail, with nil, if there's no
REPL available"
  [state code eval-opts]
  (p/let [editor-data (-> @state :editor/callbacks :editor-data)
          auto-eval-opts (when (:auto-detect eval-opts)
                           (auto-opts editor-data))
          eval-opts (merge auto-eval-opts eval-opts)]
    (if-let [repl (repl-for state (:filename eval-opts) (:aux eval-opts))]
      (eval/eval repl code eval-opts)
      (js/Promise. (fn [_ fail] (fail nil))))))

(defn- format-test-result [{:keys [test pass fail error]}]
  (str "Ran " test " test"
       (when-not (= 1 test) "s")
       (when-not (zero? pass)
         (str ", " pass " assertion"
              (when-not (= 1 pass) "s")
              " passed"))
       (when-not (zero? fail)
         (str ", " fail " failed"))
       (when-not (zero? error)
         (str ", " error " errored"))
       "."))

(defn run-tests-in-ns! [state]
  (p/let [{:keys [result]} (cmds/run-feature! state :eval
                                              {:auto-detect true :text "(clojure.test/run-tests)"})]
    (cmds/run-callback! state :notify
                        {:type (if (= 0 (:fail result) (:error result))
                                 :info
                                 :warning)
                         :title "(clojure.test/run-tests)"
                         :message (format-test-result result)})))

(defn run-test-at-cursor! [state {:keys [range contents]}]
  (let [[_ current-var] (helpers/current-var contents (first range))]
    (p/do!
     (cmds/run-feature! state :eval
                        {:auto-detect true
                         :text (str "(clojure.test/test-vars [#'" current-var "])")})
     (cmds/run-callback! state :notify {:type :info
                                        :title (str "Ran test: " current-var)
                                        :message "See REPL for any failures"}))))

(defn- source! [state]
  (p/let [eql (-> @state :editor/features :eql)
          on-stdout (-> @state :editor/callbacks :on-stdout)
          source (eql [{:editor/contents [{:text/current-var [:text/contents
                                                              :definition/source]}]}])]
    (if-let [source (-> source
                        :editor/contents
                        :text/current-var
                        :definition/source
                        :text/contents)]
      (on-stdout source)
      (helpers/with-out
        #(p/let [req (eql {:text/contents (str "(do (clojure.core/require 'clojure.repl)"
                                               " #'clojure.repl/source)")}
                          [:repl/result])]
          (if (:repl/result req)
            (eql (-> source :editor/contents :text/current-var)
                 ['(:repl/result
                    {:repl/template
                     (clojure.repl/source :repl/code)})])
            (throw "Error")))))))

(defn source-for-var! [state]
  (p/catch (source! state)
           #(cmds/run-callback! state
                                :notify
                                {:type :error
                                 :title (str "Source for Var "
                                             "not supported for "
                                             (-> @state :repl/info :kind-name))})))
