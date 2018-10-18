(ns repl-tooling.repl-client.clojurescript
  (:require [repl-tooling.repl-client :as client]
            [cljs.core.async :as async :refer-macros [go go-loop]]
            [cljs.reader :as reader]
            [repl-tooling.eval :as eval]))

(defn evaluate-code [in pending command opts callback]
  (let [id (gensym)
        code (str "(pr-str (try (clojure.core/let [res " command
                  "\n] ['" id " :result (pr-str res)]) (catch :default e "
                  "['" id " :error (pr-str {:obj e :type (.-type e) "
                  ":message (.-message e) :trace (.-stack e)})])))\n")]
    (swap! pending assoc id callback)
    (when-let [ns-name (:namespace opts)]
      (async/put! in (str "(ns " ns-name ")")))
    (async/put! in code)
    id))

(defrecord Evaluator [in pending]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (evaluate-code in pending command opts callback))
  (break [this id]))

(defn- treat-result-of-call [out pending output-fn]
  (if-let [callback (and (vector? out) (some->> out first (get @pending)))]
    (let [[id key parsed] out]
      (callback {key parsed})
      (swap! pending dissoc id)
      (output-fn {:as-text out :result parsed}))
    (output-fn {:out out})))

(defn- pending-evals [pending output-fn out]
  (try
    (treat-result-of-call (-> out reader/read-string reader/read-string)
                          pending output-fn)
    (catch :defaul _
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
    (->Evaluator in pending-cmds)))
