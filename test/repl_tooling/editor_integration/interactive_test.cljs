(ns repl-tooling.editor-integration.interactive-test
  (:require [clojure.string :as str]
            [repl-tooling.editor-integration.interactive :as int]
            [reagent.core :as r]
            [clojure.test :refer [testing]]
            [check.core :refer-macros [check]]
            [check.async :refer-macros [async-test]]
            [repl-tooling.eval-helpers :refer-macros [wait-for-change]]
            [repl-tooling.integration.ui-macros :as m :include-macros true]
            [repl-tooling.eval-helpers :as e]
            [clojure.core.async :as async]
            [devcards.core :as cards]))

(m/card-for-renderer!)
(defn render [interactive-obj repl]
  (let [obj (int/->Interactive interactive-obj repl {})]
    (reset! state obj)
    (m/text-on-result)))

(cards/deftest interactive-renderer
  (reset! state nil)
  (e/async-with-repl "will render a hiccup, based on a state"
    (render '{:html [:div ?state] :state 20} repl)

    (testing "renders initial state"
      (check (wait-for-change m/text-on-result) => {:text "20" :html "<div>20</div>"}))

    (testing "updates initial state with fn"
      (render '{:html [:a {:href "#" :on-click ?inc} ?state]
                :state 20
                :fns {:inc (fn [event state] (inc state))}}
              repl)
      (check (wait-for-change m/text-on-result) => {:text "20"})
      (m/click-on "20")
      (check (wait-for-change m/text-on-result) => {:text "21"}))))
