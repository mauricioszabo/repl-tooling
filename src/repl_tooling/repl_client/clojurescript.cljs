(ns repl-tooling.repl-client.clojurescript
  (:require [repl-tooling.repl-client :as client]
            [cljs.core.async :as async :refer-macros [go go-loop]]
            [cljs.reader :as reader]
            [repl-tooling.eval :as eval]
            [repl-tooling.features.autocomplete :as f-auto]
            [repl-tooling.repl-client.cljs.autocomplete :as cljs-auto]
            [repl-tooling.editor-helpers :as helpers]))

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

(defn- generic-autocomplete [repl ns-name prefix]
  (js/Promise. (fn [resolve]
                 (cljs-auto/complete repl
                                     ns-name
                                     prefix
                                     #(if-let [res (:result %)]
                                        (resolve (helpers/read-result res))
                                        (resolve []))))))

(defn- lumo-autocomplete [repl ns-name prefix]
  (js/Promise. (fn [resolve]
                 (eval/evaluate repl
                                `(lumo.repl/get-completions ~prefix cljs.core/js->clj)
                                {:namespace ns-name :ignore true}
                                #(if-let [res (:result %)]
                                   (resolve (helpers/read-result res))
                                   (resolve []))))))

(defn- detect-autocomplete [repl ns-name text prefix row col state]
  (let [treat (fn [{:keys [result]}]
                (if result
                  (swap! state assoc :autocomplete-kind :lumo)
                  (swap! state assoc :autocomplete-kind :generic)))]
    (.
      (js/Promise. (fn [resolve]
                     (eval/evaluate repl
                                    "(require 'lumo.repl)"
                                    {:ignore true}
                                    treat)))
      (then (fn []
              (f-auto/complete repl ns-name text prefix row col))))))

(defrecord Evaluator [in pending state]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (evaluate-code in pending command opts callback))
  (break [this id])

  f-auto/AutoComplete
  (complete [repl ns-name text prefix row col]
    (case (:autocomplete-kind @state)
      nil (detect-autocomplete repl ns-name text prefix row col state)
      :lumo (lumo-autocomplete repl ns-name prefix)
      :generic (generic-autocomplete repl ns-name prefix))))

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
    (->Evaluator in pending-cmds (atom {}))))
