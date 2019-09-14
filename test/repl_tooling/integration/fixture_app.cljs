(ns repl-tooling.integration.fixture-app
  (:require [clojure.string :as st]))

(def ^:private private-var 20)
(def local-var 10)

(defn- provate-fn [a b] (+ a b 11))
(defn local-fn [a b] (+ a b 10))
(defn main []
  (prn :CONNECTED))
