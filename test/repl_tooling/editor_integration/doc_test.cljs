(ns repl-tooling.editor-integration.doc-test
  (:require [clojure.string :as str]
            [repl-tooling.editor-integration.doc :as doc]
            [repl-tooling.editor-integration.interactive :as int]
            [repl-tooling.editor-integration.renderer :as render]
            [repl-tooling.editor-helpers :as helpers]
            [reagent.core :as r]
            [repl-tooling.eval :as repl]
            [clojure.test :refer [testing]]
            [check.core :refer-macros [check]]
            [check.async :refer-macros [async-test]]
            [repl-tooling.eval-helpers :as h :include-macros true]
            [repl-tooling.integration.ui-macros :as m :include-macros true]
            [clojure.core.async :as async]
            [devcards.core :as cards]
            [repl-tooling.commands-to-repl.doc-and-spec :as sdoc]))

(m/card-for-renderer!)

(defonce future-eval-res (atom nil))
(defn evaluate-to [res] (reset! future-eval-res res))
(defn e []
  (.resolve js/Promise [:replace [:html [:div @future-eval-res]]]))

(defn render [repl spec]
  (.then (doc/describe-spec repl spec)
         #(let [p (render/parse-result % repl (atom {:editor/features
                                                     {:eval (fn [code opts]
                                                              (repl/eval repl code opts))}}))]
            (reset! state @p))))

; TODO: Make this work later, when we have a way to run recursive code
#_
(cards/deftest rendering-specs
  (h/async-with-repl "Rendering specs in a sane way"
    (testing "rendering a 'leaf' spec"
      (render repl "string?")
      (check (h/wait-for-change m/text-on-result) => {:text "string?"}))

    (testing "rendering a 'map' spec"
      (render repl "(keys :req-un [:doc/a-str])")
      (check (h/wait-for-change m/text-on-result) => {:text "{ :a-str :doc/a-str }"}))))

      ; (evaluate-to 'string?)
      ; (m/click-on "required")
      ; (check (wait-for-change m/text-on-result) => {:text "{ :a-str string? }"}))))
