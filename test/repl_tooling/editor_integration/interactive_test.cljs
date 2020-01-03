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

(defn- render
  ([interactive-obj] (render interactive-obj {}))
  ([interactive-obj editor-features]
   (let [obj (int/->Interactive interactive-obj nil editor-features)]
     (reset! state obj)
     (text-on-result))))

(defn- click-on [link-label]
  (when-let [elem (->> (. js/document querySelectorAll "a")
                       js/Array.prototype.slice.call
                       (filter #(= (.-innerText %) link-label))
                       first)]
    (.click elem)))

(def eval-data {:range [[1 1] [1 10]]
                :editor-data {:filename "somecode.cljs"
                              :range [[1 1] [1 20]]
                              :contents "(ns lol)\nsome source code"}})
(defn- ensure-eval [code opts]
  (assert (= code "some-right-code"))
  (assert (= opts {:filename "somecode.cljs"
                   :range [[1 1] [1 10]]
                   :namespace "lol"
                   :ignore true
                   :pass {:interactive true}}))
  (.resolve js/Promise [:replace [:html [:div 3]]]))

(cards/deftest rendering-elements
  (reset! state nil)
  (async-test "interactive renderer" {:timeout 8000}
    (testing "rendering :render - like normal tooling renderer"
      (render [:render {:foo 10}])
      (check (wait-for-change text-on-result) => {:text "{ :foo 10 }"})
      (click-on "")
      (check (wait-for-change text-on-result) => {:text "{ :foo 10 } [ :foo 10 ]"}))

    (testing "rendering HTML elements"
      (render [:html [:div "LOL"]])
      (check (wait-for-change text-on-result) => {:text "LOL"
                                                  :html "<div>LOL</div>"}))

    (testing "rendering HTML replacement elements"
      (render [:html [:a {:href "#" :on-click [:replace [:html [:div "New"]]]} "old"]])
      (wait-for-change text-on-result)
      (click-on "old")
      (wait-for-change text-on-result)
      (check (text-on-result) => {:text "New"}))

    (testing "rendering HTML mixed with default render"
      (render [:html [:div [:render {:foo 10}]]])
      (check (wait-for-change text-on-result) => {:text "{ :foo 10 }"})
      (click-on "")
      (check (wait-for-change text-on-result) => {:text "{ :foo 10 } [ :foo 10 ]"}))

    (testing "re-eval and dispatch"
      (render [:html [:a {:href "#" :on-click [:eval "some-right-code"]}
                      "eval"]]
              (with-meta (r/atom {:editor/features {:eval ensure-eval}})
                         eval-data))

      (wait-for-change text-on-result)
      (click-on "eval")
      (check (wait-for-change text-on-result) => {:text "3"}))))
