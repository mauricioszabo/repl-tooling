(ns repl-tooling.repl-client.clojurescript
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [cljs-blob-contents]])
  (:require [repl-tooling.repl-client :as client]
            [cljs.core.async :as async :refer-macros [go go-loop]]
            [cljs.reader :as reader]
            [repl-tooling.eval :as eval]
            ; [repl-tooling.features.autocomplete :as f-auto]
            [repl-tooling.editor-helpers :as helpers]))

(def blob (cljs-blob-contents))

(defn- lumo-autocomplete [repl ns-name prefix]
  (js/Promise. (fn [resolve]
                 (eval/evaluate repl
                                `(lumo.repl/get-completions ~prefix cljs.core/js->clj)
                                {:namespace ns-name :ignore true}
                                #(if-let [res (:result %)]
                                   (resolve (helpers/read-result res))
                                   (resolve []))))))

(defrecord Evaluator [in pending state]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (let [id (gensym)
          code (str "(cljs.core/pr-str (try (clojure.core/let [res\n(do\n" command
                    "\n)] ['" id " :result (cljs.core/pr-str res)]) (catch :default e "
                    "['" id " :error (cljs.core/pr-str e)])))\n")]
      (swap! pending assoc id {:callback callback :opts opts})
      (when-let [ns-name (:namespace opts)] (async/put! in (str "(ns " ns-name ")")))
      (async/put! in code)
      id))
  (break [this id]))

(defn- treat-result-of-call [out pending output-fn]
  (if-let [pendency (and (vector? out) (some->> out first (get @pending)))]
    (let [[id key parsed] out
          opts (:opts pendency)
          ignore? (:ignore opts)
          result (merge {:as-text out key parsed}
                        (:pass opts))]
      ((:callback pendency) result)
      (swap! pending dissoc id)
      (when-not ignore? (output-fn result)))
    (output-fn {:out out})))

(defn- pending-evals [pending output-fn out]
  (try
    (treat-result-of-call (-> out reader/read-string reader/read-string)
                          pending output-fn)
    (catch :default _
      (output-fn {:out out}))))

(defn repl [session-name host port on-output]
  (let [[in out] (client/socket! session-name host port)
        pending-cmds (atom {})]
    (async/go-loop []
      (if-let [output (async/<! out)]
        (do
          (pending-evals pending-cmds on-output output)
          (recur))
        (on-output nil)))
    (let [evaluator (->Evaluator in pending-cmds (atom {}))]
      (eval/evaluate evaluator blob {} #(prn :EVAL-RES %))
      evaluator)))
