(ns repl-tooling.features.autocomplete.suitable
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [repl-tooling.eval :as eval]))

(defn- re-escape [str]
  (str/replace str #"[.*+?^${}()|\[\]\\]" "\\$&"))

(defn- make-context [text prefix row col]
  (let [lines (str/split-lines text)
        pattern (re-pattern (str "(.{" (- col (count prefix)) "})" (re-escape prefix)))]
    (->> "$1__prefix__"
         (update lines row str/replace-first pattern)
         (str/join "\n"))))

(defn- cmd-to-run-cljs [shadow-env]
  `(~'clojure.core/fn [ns# code#]
     (~'clojure.core/let [res#
                          (shadow.cljs.devtools.api/cljs-eval
                           ~shadow-env code# {:ns (~'clojure.core/symbol ns#)})]
       (if-let [results# (:results res#)]
        {:value (clojure.edn/read-string (~'clojure.core/last results#))}
        {:error res#}))))

(defn- compliment [repl prefix cmd-for-cljs-env ns context]
  (let [code `(do
                (~'clojure.core/require 'suitable.compliment.sources.cljs)
                (~'clojure.core/binding [suitable.compliment.sources.cljs/*compiler-env*
                                         ~cmd-for-cljs-env]
                  (suitable.compliment.sources.cljs/candidates ~prefix
                                                               '~ns
                                                               ~context)))]
    (-> (eval/eval repl code)
        (p/then :result)
        (p/catch (constantly [])))))

(defn- suitable [repl prefix shadow-env ns context]
  (let [code `(do
                (~'clojure.core/require
                  'shadow.cljs.devtools.api
                  'clojure.edn)
                (suitable.js-completions/cljs-completions
                  ~(cmd-to-run-cljs shadow-env)
                  ~prefix
                  {:ns (~'clojure.core/str ~ns)
                   :context ~context}))]
    (-> (eval/eval repl code)
        (p/then :result)
        (p/catch (constantly [])))))

(defn for-cljs [repl aux shadow-env cmd-for-cljs-env ns-name text prefix row col]
  (let [ns (when ns-name (str ns-name))
        context (make-context text prefix row col)
        suitable (suitable aux prefix shadow-env ns context)
        compliment (compliment aux prefix cmd-for-cljs-env ns context)]
    (-> (p/all [suitable compliment])
        (p/then #(apply concat %))
        (p/then distinct))))
