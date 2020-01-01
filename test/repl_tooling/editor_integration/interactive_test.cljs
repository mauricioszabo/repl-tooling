(ns repl-tooling.editor-integration.interactive-test
  (:require [clojure.string :as str]
            [repl-tooling.editor-integration.interactive :as int]
            [repl-tooling.editor-helpers :as helpers]
            [reagent.core :as r]
            [clojure.test :refer [testing]]
            [check.core :refer-macros [check]]
            [devcards.core :as cards]))

(cards/defcard render-viewport
  "<div class='result'>Awaiting for result</div>")

(defn- text-on-result []
  (let [elem (. js/document querySelector "div.result")]
    {:text (-> elem .-innerText (str/replace #"\n" " "))
     :html (.-innerHTML elem)}))


(defn- render [interactive-obj]
  (let [obj (int/->Interactive interactive-obj nil {})
        html (helpers/as-html obj (r/atom obj) true)
        elem (. js/document querySelector "div.result")]
    (r/render html elem)
    (text-on-result)))

(defn- click-on [link-label]
  (when-let [elem (->> (. js/document querySelectorAll "a")
                       js/Array.prototype.slice.call
                       (filter #(= (.-innerText %) link-label))
                       first)]
    (.click elem)))

(cards/deftest rendering-elements
  (testing "rendering :render - like normal tooling renderer"
    (check (render [:render {:foo 10}]) => {:text "{ :foo 10 }"}))

  (testing "rendering HTML elements"
    (check (render [:html [:div "LOL"]]) => {:text "LOL"
                                             :html "<div>LOL</div>"}))

  (testing "rendering HTML replacement elements"
    (render [:html [:a {:href "#" :on-click [:replace [:html [:div "New"]]]} "old"]])
    (click-on "old")
    (check (text-on-result) => {:text "New"})))
