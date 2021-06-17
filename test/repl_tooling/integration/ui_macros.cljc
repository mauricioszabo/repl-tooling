(ns repl-tooling.integration.ui-macros
  #?(:cljs (:require-macros [repl-tooling.integration.ui-macros]))
  (:require [clojure.core.async :as async]
            [clojure.string :as str]
            [reagent.core :as r]
            [devcards.core :as cards :include-macros true]
            #?(:cljs [repl-tooling.editor-integration.renderer.protocols :as proto])))

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

(defmacro type-and-assert-result [representation cmd]
  `(do
     (repl-tooling.integration.fake-editor/type-and-eval ~cmd)
     (promesa.core/let [res# (repl-tooling.integration.fake-editor/change-result-p)]
       (check.async/check (str/replace res# #"(\n|\s+)+" " ") ~'=> ~representation))))

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

(defmacro click-link-and-assert [representation nth]
  `(promesa.core/do!
    (.. ~'js/document
        (~'querySelector ~(str "#result a:nth-child(n+" nth ")"))
        ~'click)

    (promesa.core/let [res# (repl-tooling.integration.fake-editor/change-result-p)]
      (check.async/check (str/replace res# #"(\n|\s+)+" " ") ~'=> ~representation))))

(defmacro click-link-and-assert-children [representation nth]
  `(promesa.core/do!
     (.. ~'js/document
         (~'querySelector ~(str "#result a:nth-child(n+" nth ")"))
         ~'click)

     (repl-tooling.integration.fake-editor/change-result-p)
     (promesa.core/let [res# (repl-tooling.integration.fake-editor/txt-for-selector
                              "#result .children")]
        (check.async/check (str/replace res# #"(\n|\s+)+" " ") ~'=> ~representation))))

(defmacro card-for-renderer! []
  `(do
    (defonce ~'state (r/atom nil))
    (defn ~'result []
      (if-let [obj# @~'state]
        (let [html# (repl-tooling.editor-integration.renderer.protocols/as-html obj# ~'state true)]
          [:div.result html#])
        [:div.result "Waiting for result"]))

    (cards/defcard-rg ~'render-viewport
      [~'result])))

#?(:cljs
   (defn text-on-result []
     (let [elem (. js/document querySelector "div.result")]
       {:text (-> elem .-innerText (str/replace #"\n" " "))
        :html (.-innerHTML elem)})))

#?(:cljs
   (defn click-on [link-label]
     (when-let [elem (->> (. js/document querySelectorAll "a")
                          js/Array.prototype.slice.call
                          (filter #(= (.-innerText %) link-label))
                          first)]
       (.click elem))))
