(ns repl-tooling.editor-integration.evaluation
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.schemas :as schemas]
            [schema.core :as s]))

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

(defn repl-for [opts state filename aux?]
  (let [cljs? (need-cljs? ((:get-config opts)) filename)
        repl (cond
               cljs? (:cljs/repl @state)
               aux? (:clj/aux @state)
               :else (:clj/repl @state))]
    (if (nil? repl)
      (treat-error (:notify opts) cljs? (:clj/repl @state))
      repl)))

(s/defn eval-cmd [state
                  code :- s/Str
                  namespace
                  range :- schemas/Range
                  editor-data :- schemas/EditorData
                  opts]
  (when code
    (let [filename (:filename editor-data)
          {:keys [on-start-eval on-eval]} opts
          [[row col]] range
          repl (repl-for opts state filename false)
          id (gensym)
          eval-data {:id id
                     :editor-data editor-data
                     :range range}]
      (when repl
        (and on-start-eval (on-start-eval eval-data))
        (eval/evaluate repl
                       code
                       {:filename filename
                        :id id
                        :row (inc row)
                        :col (inc col)
                        :namespace namespace
                        ;; FIXME: this is kinda bad, we're re-using opts...
                        :pass (:pass opts)}
                       #(when (and on-eval @state)
                          (on-eval (assoc eval-data
                                          :repl repl
                                          :result (helpers/parse-result %)))))))))

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
  [state opts code eval-opts]
  (p/let [editor-data (-> @state :editor/callbacks :editor-data)
          auto-eval-opts (when (:auto-detect eval-opts)
                           (auto-opts editor-data))
          eval-opts (merge auto-eval-opts eval-opts)]
    (if-let [repl (repl-for opts state (:filename eval-opts) (:aux eval-opts))]
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
  (let [notify (-> @state :editor/callbacks :notify)
        evaluate (-> @state :editor/features :eval)]
    (p/let [res (evaluate "(clojure.test/run-tests)"
                          {:auto-detect true})]
      (notify {:type :info
               :title "(clojure.test/run-tests)"
               :message (format-test-result (:result res))}))))

(defn run-test-at-cursor! [state {:keys [range contents]}]
  (let [notify (-> @state :editor/callbacks :notify)
        evaluate (-> @state :editor/features :eval)
        [_ current-var] (helpers/current-var contents (first range))]
    (p/do!
     (evaluate (str "(clojure.test/test-vars [#'" current-var "])")
               {:auto-detect true}
      (notify {:type :info
               :title (str "Ran test: " current-var)
               :message "See REPL for any failures"})))))

(defn source-for-var! [state {:keys [filename range contents]}]
  (let [notify (-> @state :editor/callbacks :notify)
        get-config (-> @state :editor/callbacks :get-config)
        evaluate (-> @state :editor/features :eval)
        [_ current-var] (helpers/current-var contents (first range))
        opts {:auto-detect true}]

    (if (need-cljs? (get-config) filename)
      (notify {:type :error :title "Source for Var not supported for ClojureScript"})
      (-> (evaluate "(require 'clojure.repl)" opts)
          (p/then #(evaluate (str "(clojure.repl/source " current-var ")") opts))
          (p/catch #(notify {:type :error :title (str "Source for Var "
                                                      "not supported for "
                                                      (-> @state :repl/info :kind-name))}))))))
