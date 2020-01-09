(ns repl-tooling.editor-integration.doc-test
  (:require [clojure.string :as str]
            [repl-tooling.editor-integration.doc :as doc]
            [repl-tooling.editor-integration.interactive :as int]
            [repl-tooling.editor-helpers :as helpers]
            [reagent.core :as r]
            [clojure.test :refer [testing]]
            [check.core :refer-macros [check]]
            [check.async :refer-macros [async-test]]
            [repl-tooling.eval-helpers :refer-macros [wait-for-change]]
            [repl-tooling.integration.ui-macros :as m :include-macros true]
            [clojure.core.async :as async]
            [devcards.core :as cards]
            [repl-tooling.commands-to-repl.doc-and-spec :as sdoc]

            [clojure.spec.alpha :as sp]))

(sp/def ::lol string?)
(sp/def ::some-map (sp/keys :req-un [::lol]))
(sp/describe ::some-map)
(sp/describe ::lol)

(m/card-for-renderer!)

(defonce future-eval-res (atom nil))
(defn evaluate-to [res] (reset! future-eval-res res))
(defn e []
  (.resolve js/Promise [:replace [:html [:div @future-eval-res]]]))

(defn render [spec]
  (let [html (sdoc/spec2interactive spec)
        obj (int/->Interactive [:html html] nil (r/atom {:editor/features {:eval e}}))]
    (reset! state obj)))

(cards/deftest rendering-specs
  (async-test "Rendering specs on a sane way"
    (testing "rendering a 'leaf' spec"
      (check (sdoc/spec2interactive 'string?) => [:div.other "string?"]))

    (testing "rendering a 'map' spec"
      (render '(keys :req-un [:doc/a-str]))
      (check (wait-for-change m/text-on-result) => {:text "{ :a-str required }"}))))

      ; (evaluate-to 'string?)
      ; (m/click-on "required")
      ; (check (wait-for-change m/text-on-result) => {:text "{ :a-str string? }"}))))
