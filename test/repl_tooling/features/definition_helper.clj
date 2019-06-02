(ns repl-tooling.features.definition-helper
  (:require [repl-tooling.features.definition-child :as c :refer [other-var]]))

(defn some-function []
  (str "Just a function " other-var))
