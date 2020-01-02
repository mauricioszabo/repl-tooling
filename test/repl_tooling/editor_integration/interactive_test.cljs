(ns repl-tooling.editor-integration.interactive-test
  (:require [clojure.string :as str]
            [repl-tooling.editor-integration.interactive :as int]
            [repl-tooling.editor-helpers :as helpers]
            [reagent.core :as r]
            [clojure.test :refer [testing]]
            [check.core :refer-macros [check]]
            [check.async :refer-macros [async-test]]
            [repl-tooling.eval-helpers :refer-macros [wait-for-change]]
            [clojure.core.async :as async]
            [devcards.core :as cards]))

(defonce state (r/atom nil))
(defn result []
  (if-let [obj @state]
    (let [html (helpers/as-html obj state true)]
      [:div.result html])
    [:div.result "Waiting for result"]))
;   (prn :state state)

(cards/defcard-rg render-viewport
  [result])
#_
(render [:html [:div "FOO"]])

(defn- text-on-result []
  (let [elem (. js/document querySelector "div.result")]
    {:text (-> elem .-innerText (str/replace #"\n" " "))
     :html (.-innerHTML elem)}))

(defn- render [interactive-obj]
  (let [obj (int/->Interactive interactive-obj nil {})]
    (reset! state obj)
    (text-on-result)))

(defn- click-on [link-label]
  (when-let [elem (->> (. js/document querySelectorAll "a")
                       js/Array.prototype.slice.call
                       (filter #(= (.-innerText %) link-label))
                       first)]
    (.click elem)))

(cards/deftest rendering-elements
  (reset! state nil)
  (async-test "interactive renderer"
    (testing "rendering :render - like normal tooling renderer"
      (render [:render {:foo 10}])
      (check (wait-for-change text-on-result) => {:text "{ :foo 10 }"})
      (click-on "")
      (check (wait-for-change text-on-result) => {:text "{ :foo 10 } [ :foo 10 ]"}))

    (testing "rendering HTML elements"
      (render [:html [:div "LOL"]])
      (check (wait-for-change text-on-result) => {:text "LOL"
                                                  :html "<div>LOL</div>"})))


  #_
  (testing "rendering HTML replacement elements"
    (render [:html [:a {:href "#" :on-click [:replace [:html [:div "New"]]]} "old"]])
    (click-on "old")
    (check (text-on-result) => {:text "New"})))
