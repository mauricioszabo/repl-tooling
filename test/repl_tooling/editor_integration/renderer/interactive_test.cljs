(ns repl-tooling.editor-integration.renderer.interactive-test
  (:require [clojure.string :as str]
            [repl-tooling.editor-integration.renderer.interactive :as int]
            ; [reagent.core :as r]
            [repl-tooling.integration.fake-editor :as fake]
            [clojure.test]
            ; [check.core :refer [check]]
            ; [check.async-old :refer [async-test]]
            [check.async :refer [check async-test testing]]
            [repl-tooling.integration.ui-macros :as m]
            [repl-tooling.eval-helpers :as e]
            [clojure.core.async :as async]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.editor-integration.configs :as configs]))

(defn render [interactive-obj]
  (let [text (str "'" (pr-str interactive-obj))]
    (fake/type text)
    (fake/run-feature! :evaluate-and-render
                       {:text text
                        :range [[0 0] [0 0]]
                        :pass {:interactive true
                               :aux true}})))

(cards/deftest interactive-renderer
  (async-test "pathom resolver customization" {:teardown (fake/disconnect!)
                                               :timeout 16000}
    (fake/connect!)
    (testing "renders initial state"
      (render '{:html [:div ?state] :state 10})
      (check (fake/change-result-p) => "10"))

    (testing "updates initial state with fn"
      (render '{:html [:a {:href "#" :on-click ?inc} ?state]
                :state 20
                :fns {:inc (fn [event state] (inc state))}})
      (check (fake/change-result-p) => "20")
      (m/click-on "20")
      (check (fake/change-result-p) => "21"))

    (testing "maps in states"
      (render '{:html [:div (:val ?state)]
                :state {:val 20}})
      (check (fake/change-result-p) => "20"))

    (testing "nested state"
      (render '{:html [:div (:val (:v ?state))]
                :state {:v {:val 21}}})
      (check (fake/change-result-p) => "21"))

    (testing "code on HTML"
      (render '{:html [:div [:div (map #(vector :span (inc %)) (:vec ?state))]
                            [:div (pr-str (walk/postwalk-replace {1 2} (:nested ?state)))]]
                :state {:vec [1 2 3]
                        :nested {:some {:nested [1 1]}}}})
      (check (fake/change-result-p) => "2\n3\n4\n{:some {:nested [2 2]}}"))

    (testing "passing params to callback fns"
      (render '{:html [:a {:href "#" :on-click (?inc :n)} (:n ?state)]
                :state {:n 20}
                :fns {:inc (fn [event state key] (update state key inc))}})
      (check (fake/change-result-p) => "20")
      (m/click-on "20")
      (check (fake/change-result-p) => "21"))

    (testing "renders tooling's EDN renderer"
      (configs/register-custom-tags! {})
      (render '{:html [:div/clj '(+ 1 2 3 4)]})
      (check (fake/change-result-p) => "(\n+\n \n1\n \n2\n \n3\n \n4\n)"))))

(cards/defcard-rg fake-editor
 fake/editor
 fake/state)
