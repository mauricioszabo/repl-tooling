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
    (testing "renders initial state"
      (render '{:html [:div ?state] :state 20} repl)
      (check (wait-for-change m/text-on-result) => {:text "20" :html "<div>20</div>"}))

    (testing "updates initial state with fn"
      (render '{:html [:a {:href "#" :on-click ?inc} ?state]
                :state 20
                :fns {:inc (fn [event state] (inc state))}}
              repl)
      (check (wait-for-change m/text-on-result) => {:text "20"})
      (m/click-on "20")
      (check (wait-for-change m/text-on-result) => {:text "21"}))

    (testing "maps in states"
      (render '{:html [:div (:val ?state)]
                :state {:val 20}}
              repl)
      (check (wait-for-change m/text-on-result) => {:text "20"}))

    (testing "nested state"
      (render '{:html [:div (:val (:v ?state))]
                :state {:v {:val 20}}}
              repl)
      (check (wait-for-change m/text-on-result) => {:text "20"}))

    (testing "code on HTML"
      (render '{:html [:div [:div (map #(vector :span (inc %)) (:vec ?state))]
                            [:div (pr-str (walk/postwalk-replace {1 2} (:nested ?state)))]]
                :state {:vec [1 2 3]
                        :nested {:some {:nested [1 1]}}}}
              repl)
      (check (wait-for-change m/text-on-result)
             => {:html (str "<div>"
                            "<div><span>2</span><span>3</span><span>4</span></div>"
                            "<div>{:some {:nested [2 2]}}</div>"
                            "</div>")}))

    (testing "passing params to callback fns"
      (render '{:html [:a {:href "#" :on-click (?inc :n)} (:n ?state)]
                :state {:n 20}
                :fns {:inc (fn [event state key] (update state key inc))}}
              repl)
      (check (wait-for-change m/text-on-result) => {:text "20"})
      (m/click-on "20")
      (check (wait-for-change m/text-on-result) => {:text "21"}))))
