(ns repl-tooling.features.autocomplete
  (:require [repl-tooling.eval :as eval]
            [cljs.core.async :refer [<! >!] :refer-macros [go] :as async]
            [repl-tooling.repl-client :as client]
            [repl-tooling.repl-client.lumo :as lumo]))

(defn- detect-fn
  ([evaluator fun-name check-for key fun]
   (js/Promise.
    (fn [resolve]
      (eval/eval evaluator
                 fun-name
                 #(resolve (when (re-find check-for %)
                             {key fun})))))))

(defn- lumo-fn [evaluator ns-name text callback]
  (let [ns-form (str "(in-ns '" ns-name ")")
        complete-form `(lumo.repl/get-completions
                        ~text cljs.core/js->clj)]
    (eval/eval evaluator
               (str ns-form complete-form)
               callback)))

(defn- merge-all [ & features])

(defn detect [evaluator callback]
  (def evaluator evaluator)
  (-> (detect-fn evaluator "lumo.repl/get-completions" #"function "
                 :simple-complete lumo-fn)
      (.then callback)
      (.catch #(. js/console (log %)))))

; (-> (detect-fn evaluator "lumo.repl/get-completions" #"Function"
;                  :simple-complete lumo-fn)
;     (.catch #(. js/console (log %))))
