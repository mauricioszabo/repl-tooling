(ns repl-tooling.integration.ui-macros
  (:require [clojure.core.async :as async]
            [clojure.string :as str]))

(defn- type-and-just-for-test [])

(defmacro type-and-result [cmd]
  `(do
     (~'type-and-eval ~cmd)
     (async/<! (~'change-stdout))))

(defmacro assert-out [representation cmd]
  `(do
     (~'type-and-eval ~cmd)
     (async/<! (~'change-stdout))
     (~'check (str/replace (~'txt-for-selector "#result") #"(\n|\s+)+" " ") ~'=> ~representation)))

(defmacro click-nth-link-and-assert [representation nth]
  `(do
     (~'click-selector ~(str "#result a:nth-child(n+" nth ")"))
     (async/<! (~'change-result))
     (~'check (str/replace (~'txt-for-selector "#result") #"(\n|\s+)+" " ")
       ~'=> ~representation)))

(defmacro click-nth-link-and-assert-children [representation nth]
  `(do
     (~'click-selector ~(str "#result a:nth-child(n+" nth ")"))
     (async/<! (~'change-result))
     (~'check (str/replace (~'txt-for-selector "#result .children") #"(\n|\s+)+" " ")
       ~'=> ~representation)))
