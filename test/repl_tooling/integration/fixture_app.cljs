(ns repl-tooling.integration.fixture-app
  (:require [clojure.string :as st :refer [replace-first]]
            [promesa.core :as p]
            [repl-tooling.target-eval-test :as test]))

(def ^:private private-var 20)
(def local-var 10)

(defn- private-fn [a b] (+ a b 11))
(defn local-fn [a b] (+ a b 10))

(defn some-replace
  "Replaces the first occurrence or Bar with nothing"
  [s]
  (replace-first s "Bar" ""))

(defn main [ & args]
  :cljs-autocomplete-keyword
  (if (-> args count zero?)
    (prn :CONNECTED)
    (test/run (first args))))
