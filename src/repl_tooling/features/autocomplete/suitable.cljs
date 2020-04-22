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

(defn- cmd-to-run-js [shadow-env]
  `(~'clojure.core/fn [nss# code#]
     (~'clojure.core/let [[v# e#] (~'clojure.core/->
                                   (shadow.cljs.devtools.server.worker/worker-request
                                    (shadow.cljs.devtools.api/get-worker ~shadow-env)
                                    {:type :repl-eval
                                     :input (~'clojure.core/str "(in-ns '" nss# ") " code#)})
                                   :results
                                   ~'clojure.core/last
                                   :result
                                   ((~'clojure.core/juxt :value :error)))]
       {:error e#
        :value (~'clojure.core/some-> v# clojure.edn/read-string)})))

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
                  'suitable.js-completions
                  'shadow.cljs.devtools.server.worker
                  'shadow.cljs.devtools.api
                  'clojure.edn)
                (suitable.js-completions/cljs-completions
                  ~(cmd-to-run-js shadow-env)
                  ~prefix
                  {:ns ~ns
                   :context ~context}))]
    (-> (eval/eval repl code)
        (p/then :result)
        (p/catch (constantly [])))))

(defn for-cljs [repl shadow-env cmd-for-cljs-env ns-name text prefix row col]
  (let [ns (when ns-name (str ns-name))
        context (make-context text prefix row col)
        suitable (suitable repl prefix shadow-env ns context)
        compliment (compliment repl prefix cmd-for-cljs-env ns context)]
    (-> (p/all [suitable compliment])
        (p/then #(apply concat %))
        (p/then distinct))))
