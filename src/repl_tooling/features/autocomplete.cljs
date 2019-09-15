(ns repl-tooling.features.autocomplete
  (:require [repl-tooling.eval :as eval]
            [clojure.string :as str]
            [repl-tooling.repl-client.cljs.autocomplete :as cljs-auto]
            [repl-tooling.repl-client.clojure :as clj-repl]
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

(extend-protocol AutoComplete
  clj-repl/Evaluator
  (complete [repl ns-name text prefix row col]
    (clj-compliment repl ns-name text prefix row col))

  clj-repl/SelfHostedCljs
  (complete [repl ns-name _text prefix _row _col]
    (js/Promise. (fn [resolve]
                   (cljs-auto/complete repl
                                       ns-name
                                       prefix
                                       #(if-let [res (:result %)]
                                          (resolve (helpers/read-result res))
                                          (resolve [])))))))
