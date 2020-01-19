(ns repl-tooling.editor-integration.evaluation
  (:require [clojure.string :as str]
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

              :clj
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
                       #(when on-eval
                          (prn :EVAL-DATA eval-data)
                          (on-eval (assoc eval-data :result (helpers/parse-result %)))))))))

(defn eval-with-promise
  "Evaluates the current code and evaluation options on the current REPL.
Accepts an extra argument on `eval-opts` that's :aux - if true, evaluates
on the 'auxiliary' REPL instead of primary. On Clojure, this means that
the code will use UNREPL but will not use ellisions on infinite sequences, etc.

Please notice that because the REPL is auto-detected, `:filename` is required.
Otherwise, ClojureScript REPL will never be used!

Will return a 'promise' that is resolved to the eval result, or failed if the
eval result is an error. It will also return a fail, with nil, if there's no
REPL available"
  [state opts code eval-opts]
  (if-let [repl (repl-for opts state (:filename opts) (:aux eval-opts))]
    (eval/eval repl code eval-opts)
    (js/Promise. (fn [_ fail] (fail nil)))))
