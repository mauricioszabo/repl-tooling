(ns repl-tooling.integration.ui-macros
  (:require [clojure.core.async :as async]
            [clojure.string :as str]
            [reagent.core :as r]
            [devcards.core :as cards]
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


; (defn evaluate-cljs [nss code]
;   (let [[v e] (->
;                (shadow.cljs.devtools.server.worker/worker-request
;                 (shadow.cljs.devtools.api/get-worker :integration)
;                 {:type :repl-eval
;                  :session-id "1234"
;                  :input (str "(in-ns '" nss ") " code)})
;                :results
;                last
;                :result
;                ((juxt :value :error)))]
;     {:error e
;      :value (some-> v clojure.edn/read-string)}))
;
; (require '[suitable.js-completions :as js-c])
; (js-c/cljs-completions evaluate-cljs
;                        "fo"
;                        {:ns "repl-tooling.features.autocomplete.compliment"
;                         :context "(let [foo 10] __prefix__ )"})
;
; (require '[suitable.compliment.sources.cljs :as suit])
; (binding [suit/*compiler-env*
;           (shadow.cljs.devtools.api/compiler-env :integration)]
;   (suit/candidates "js/go" 'cljs.user "(let [foo 10] __prefix__ )"))
