(ns repl-tooling.integration.fixture-app
  (:require [clojure.string :as st]
            [repl-tooling.target-eval-test :as test]))

(def ^:private private-var 20)
(def local-var 10)

(defn- private-fn [a b] (+ a b 11))
(defn local-fn [a b] (+ a b 10))
(defn main [ & args]
  :cljs-autocomplete-keyword
  (if (-> args count zero?)
    (prn :CONNECTED)
    (test/run (first args))))
