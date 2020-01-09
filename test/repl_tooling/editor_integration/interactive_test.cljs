(ns repl-tooling.editor-integration.interactive-test
  (:require [clojure.string :as str]
            [repl-tooling.editor-integration.interactive :as int]
            [repl-tooling.editor-helpers :as helpers]
            [reagent.core :as r]
            [clojure.test :refer [testing]]
            [check.core :refer-macros [check]]
            [check.async :refer-macros [async-test]]
            [repl-tooling.eval-helpers :refer-macros [wait-for-change]]
            [repl-tooling.integration.ui-macros :as m :include-macros true]
            [clojure.core.async :as async]
            [devcards.core :as cards]))

(m/card-for-renderer!)

(defn render
  ([interactive-obj] (render interactive-obj {}))
  ([interactive-obj editor-features]
   (let [obj (int/->Interactive interactive-obj nil editor-features)]
     (reset! state obj)
     (m/text-on-result))))

(def eval-data {:range [[1 1] [1 10]]
                :editor-data {:filename "somecode.cljs"
                              :range [[1 1] [1 20]]
                              :contents "(ns lol)\nsome source code"}})
(defn- ensure-eval [code opts]
  (assert (= code "some-right-code"))
  (assert (= opts {:ignore true :pass {:interactive true}}))
  (.resolve js/Promise [:replace [:html [:div 3]]]))

(cards/deftest rendering-elements
  (reset! state nil)
  (async-test "interactive renderer" {:timeout 8000}
    (testing "rendering :render - like normal tooling renderer"
      (render [:render {:foo 10}])
      (check (wait-for-change m/text-on-result) => {:text "{ :foo 10 }"})
      (m/click-on "")
      (check (wait-for-change m/text-on-result) => {:text "{ :foo 10 } [ :foo 10 ]"}))

    (testing "rendering HTML elements"
      (render [:html [:div "LOL"]])
      (check (wait-for-change m/text-on-result) => {:text "LOL"
                                                    :html "<div>LOL</div>"}))

    (testing "rendering HTML replacement elements"
      (render [:html [:a {:href "#" :on-click [:replace [:html [:div "New"]]]} "old"]])
      (wait-for-change m/text-on-result)
      (m/click-on "old")
      (wait-for-change m/text-on-result)
      (check (m/text-on-result) => {:text "New"}))

    (testing "deeply rendering HTML elements"
      (render [:html
               [:div
                [:div "Keep this "]
                [:interactive
                 [:html [:div [:a {:href "#" :on-click [:replace [:html [:div "New"]]]}
                               "old"]]]]]])

      (wait-for-change m/text-on-result))
      ; (m/click-on "old")
      ; (wait-for-change m/text-on-result)
      ; (check (m/text-on-result) => {:text "Keep this New"}))

    #_
    (testing "rendering HTML mixed with default render"
      (render [:html [:div [:render {:foo 10}]]])
      (check (wait-for-change m/text-on-result) => {:text "{ :foo 10 }"})
      (m/click-on "")
      (check (wait-for-change m/text-on-result) => {:text "{ :foo 10 } [ :foo 10 ]"}))

    #_
    (testing "re-eval and dispatch"
      (render [:html [:a {:href "#" :on-click [:eval "some-right-code"]}
                      "eval"]]
              (r/atom {:editor/features {:eval ensure-eval}}))

      (wait-for-change m/text-on-result)
      (m/click-on "eval")
      (check (wait-for-change m/text-on-result) => {:text "3"}))))
