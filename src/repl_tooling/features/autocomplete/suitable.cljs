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

(defn- cmd-to-run-js [client-id]
  `(~'clojure.core/fn [ns# code#]
     (~'clojure.core/let [relay# (:relay (shadow.cljs.devtools.server.runtime/get-instance))
                          tool-in# (clojure.core.async/chan 10)
                          tool-out# (clojure.core.async/chan 3)]
       (try
         (shadow.remote.relay.api/connect relay# tool-in# tool-out# {})
         (clojure.core.async/>!! tool-in# {:op :hello :client-info {:type :repl-tooling :to "suitable"}})
         (clojure.core.async/<!! tool-out#)
         (clojure.core.async/>!! tool-in# {:op :cljs-eval
                                           :to ~client-id
                                           :input {:code code#
                                                   :ns (~'clojure.core/symbol ns#)}})
         (~'clojure.core/let [msg# (clojure.core.async/<!! tool-out#)]
           (clojure.core.async/>!! tool-in# {:op :obj-request
                                             :to (:from msg#)
                                             :request-op :edn
                                             :oid (:ref-oid msg# (:ex-oid msg#))})
           (~'clojure.core/let [result# (:result (clojure.core.async/<!! tool-out#))]
             (if (-> msg# :op (~'clojure.core/= :eval-result-ref))
               {:value (clojure.edn/read-string result#)}
               {:error result#})))

         (finally
           (clojure.core.async/close! tool-in#)
           (clojure.core.async/close! tool-out#))))))
; (cmd-to-run-js 4)
    ; (let [read-result ((resolve 'shadow.cljs.repl/read-one) build-state (StringReader. code) {})
    ;       eval-result ((resolve 'shadow.cljs.devtools.server.worker/repl-eval) worker session-id runtime-id read-result)
    ;       [value error] (->> eval-result :results last :result ((juxt :value :error)))]
    ;   {:error error
    ;    :value (some->> value edn/read-string)})))
; (defn- cmd-to-run-js [shadow-env]
;   `(~'clojure.core/fn [nss# code#]
;      (~'clojure.core/let [[v# e#] (~'clojure.core/->
;                                    (shadow.cljs.devtools.server.worker/worker-request
;                                     (shadow.cljs.devtools.api/get-worker ~shadow-env)
;                                     {:type :repl-eval
;                                      :input (~'clojure.core/str "(in-ns '" nss# ") " code#)})
;                                    :results
;                                    ~'clojure.core/last
;                                    :result
;                                    ((~'clojure.core/juxt :value :error)))]
;        {:error e#
;         :value (~'clojure.core/some-> v# clojure.edn/read-string)})))

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

(defn- suitable [repl prefix client-id ns context]
  (let [code `(do
                (~'clojure.core/require
                  'shadow.cljs.devtools.server.runtime
                  'clojure.core.async
                  'shadow.remote.relay.api
                  'clojure.edn)
                (suitable.js-completions/cljs-completions
                  ~(cmd-to-run-js client-id)
                  ~prefix
                  {:ns (~'clojure.core/str ~ns)
                   :context ~context}))]
    (-> (eval/eval repl code)
        (p/then :result)
        (p/catch (constantly [])))))

(defn for-cljs [repl aux shadow-env cmd-for-cljs-env ns-name text prefix row col]
  (let [ns (when ns-name (str ns-name))
        context (make-context text prefix row col)
        client-id (some-> repl :state deref :build->id shadow-env first)
        suitable (and client-id (suitable aux prefix client-id ns context))
        compliment (compliment aux prefix cmd-for-cljs-env ns context)]
    (-> (p/all [suitable compliment])
        (p/then #(apply concat %))
        (p/then distinct))))
