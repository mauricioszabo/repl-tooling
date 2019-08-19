(ns repl-tooling.features.autocomplete
  (:require [repl-tooling.eval :as eval]
            [clojure.string :as str]
            [cljs.core.async :refer [<! >!] :refer-macros [go] :as async]
            [repl-tooling.repl-client :as client]
            [repl-tooling.repl-client.cljs.autocomplete :as cljs-auto]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.repl-client.lumo :as lumo]
            [repl-tooling.editor-helpers :as helpers]))

(defprotocol AutoComplete
  (complete [repl ns-name text prefix row col]))

(defn- re-escape [str]
  (str/replace str #"[.*+?^${}()|\[\]\\]" "\\$&"))

(defn- make-context [text prefix row col]
  (let [lines (str/split-lines text)
        pattern (re-pattern (str "(.{" (- col (count prefix)) "})" (re-escape prefix)))]
    (->> "$1__prefix__"
         (update lines row str/replace-first pattern)
         (str/join "\n"))))

(declare take-results)
(defn- get-more [repl resolve more acc]
  (eval/evaluate repl more {} #(take-results repl resolve acc %)))

(defn- take-results [repl resolve acc {:keys [result]}]
  (let [acc (vec (concat acc (helpers/read-result result)))
        more (-> acc last :repl-tooling/...)
        size (count acc)]
    (cond
      (-> size (> 50) (and more)) (-> acc butlast resolve)
      (-> size (< 50) (and more)) (get-more repl resolve more acc)
      :else (resolve acc))))

(defn- clj-compliment [repl ns-name text prefix row col]
  (let [ns (when ns-name (symbol ns-name))
        context (make-context text prefix row col)
        code `(do
                 (~'clojure.core/require '[compliment.core])
                 (~'clojure.core/let [completions# (compliment.core/completions
                                                     ~prefix
                                                     {:tag-candidates true
                                                      :ns '~ns
                                                      :context ~context})]
                   (~'clojure.core/vec completions#)))]
    (js/Promise. (fn [resolve]
                   (eval/evaluate repl code {:ignore true} #(if-let [res (:result %)]
                                                              (resolve (helpers/read-result res))
                                                              (resolve [])))))))

(defn- require-compliment [repl checker]
  (js/Promise. (fn [resolve]
                 (eval/evaluate repl `(~'clojure.core/require 'compliment.core) {}
                                (fn [res]
                                  (if (:error res)
                                    (reset! checker false)
                                    (reset! checker true))
                                  (resolve))))))

(extend-protocol AutoComplete
  clj-repl/Evaluator
  (complete [repl ns-name text prefix row col]
    (clj-compliment repl ns-name text prefix row col))

  clj-repl/SelfHostedCljs
  (complete [repl ns-name text prefix row col]
    (js/Promise. (fn [resolve]
                   (cljs-auto/complete repl
                                       ns-name
                                       prefix
                                       #(if-let [res (:result %)]
                                          (resolve (helpers/read-result res))
                                          (resolve [])))))))

;;;;;;;;;;;;; CUT HERE ;;;;;;;;;;;;;;

(defn- detect-fn
  ([evaluator fun-name check-for key fun]
   (js/Promise.
    (fn [resolve]
      (eval/eval evaluator
                 fun-name
                 {}
                 #(resolve (when (re-find check-for %)
                             {key fun})))))))

(defn- lumo-fn [evaluator ns-name text callback]
  (let [complete-form `(lumo.repl/get-completions
                        ~(str text) cljs.core/js->clj)]
    (eval/eval evaluator (str "(ns " ns-name ")") {}
               #(eval/eval evaluator complete-form {} callback))))

(defn- merge-all [ & features])

(defn detect [evaluator callback]
  (-> (detect-fn evaluator "lumo.repl/get-completions" #"function "
                 :simple-complete lumo-fn)
      (.then callback)
      (.catch #(. js/console (log %)))))

; (-> (detect-fn evaluator "lumo.repl/get-completions" #"Function"
;                  :simple-complete lumo-fn)
;     (.catch #(. js/console (log %))))
