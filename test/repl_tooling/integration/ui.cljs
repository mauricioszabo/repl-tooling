(ns repl-tooling.integration.ui
  (:require [reagent.core :as r]
            [repl-tooling.integration.ui-macros :as ui :include-macros true]
            [repl-tooling.editor-integration.renderer :as render]
            [clojure.test :refer [async testing is] :include-macros true]
            [check.core :refer-macros [check]]
            [check.async :include-macros true]
            [clojure.core.async :as async :include-macros true]
            [devcards.core :as cards :include-macros true]
            [clojure.string :as str]
            [repl-tooling.editor-integration.connection :as conn]
            [repl-tooling.editor-helpers-test]
            [repl-tooling.repl-client.parsing-test]))

(defonce state (r/atom {:host "localhost"
                        :port 2233
 ;                        :code "(do
 ;  (->> (range 100)
 ; (map #(vector % (range %)))
 ; (into {})))
 ; "
                        :code "(do (defrecord Foo [a b]) (->Foo (range 20) 20))"
                        ; :code "(range 100)"
                        :repls {:eval nil
                                :aux nil}
                        :commands {}
                        :stdout nil
                        :stderr nil
                        :eval-result nil}))

(defn disconnect! []
  (conn/disconnect!))

(defn handle-disconnect []
  (swap! state assoc
         :repls {:eval nil :aux nil}
         :stdout nil :stderr nil :eval-result nil
         :commands {}))

(defn- res [result]
  (swap! state assoc :eval-result (render/parse-result result (-> @state :repls :eval)))
  (swap! state update :stdout (fn [e] (str e "=> " (:as-text result) "\n"))))

(defn connect! []
  (when-not (-> @state :repls :eval)
    (.
      (conn/connect! (:host @state) (:port @state)
                     {:on-disconnect handle-disconnect
                      :on-stdout #(swap! state update :stdout (fn [e] (str e %)))
                      :on-eval res
                      :on-stderr #(swap! state update :stderr (fn [e] (str e %)))
                      :editor-data #(let [code (:code @state)
                                          lines (str/split-lines code)]
                                      {:contents code
                                       :filename "foo.clj"
                                       :range [[0 0]
                                               [(-> lines count dec) (-> lines last count dec)]]})})
      (then (fn [res]
              (swap! state assoc :repls {:eval (:clj/repl res)
                                         :aux (:clj/aux res)}
                     :commands (:editor/commands res)
                     :stdout "" :stderr ""))))))

(defn- evaluate []
  ((-> @state :commands :evaluate-selection :command)))

(defn fake-editor [state]
  [:div
   [:h4 "Socket REPL connections"]
   [:p [:b "Hostname: "] [:input {:type "text" :value (:host @state)
                                  :on-change #(->> % .-target .-value (swap! state assoc :host))}]
       [:b " Port: "] [:input {:type "text" :value (:port @state)
                               :on-change #(->> % .-target .-value int (swap! state assoc :port))}]]
   [:textarea {:style {:width "100%" :height "100px"}
               :value (:code @state)
               :on-change #(->> % .-target .-value (swap! state assoc :code))}]
   [:p
    (if (-> @state :repls :eval)
      [:span
       [:button {:on-click evaluate}
        "Evaluate"] " "
       [:button {:on-click disconnect!} "Disconnect!"]]
      [:button {:on-click connect!} "Connect!"])]
   [:p (if (-> @state :repls :eval) "Connected" "Disconnected")]
   [:div
    (when-let [res (:eval-result @state)]
      [:div
       [:h5 "RESULT"]
       [:pre
        [:div {:id "result" :class "result"}
         (render/view-for-result res (-> @state :repls :eval))]]])]
   (when-let [out (:stdout @state)]
     [:div
      [:h5 "STDOUT"]
      [:pre out]])
   (when-let [out (:stderr @state)]
     [:div
      [:h5 "STDERR"]
      [:pre out]])])

(cards/defcard-rg fake-editor
  fake-editor
  state
  {:inspect-data true})

(defn wait-for [f]
  (async/go
   (loop [t 0]
     (when (< t 100)
       (if-let [res (f)]
         res
         (do
           (async/<! (async/timeout 100))
           (recur (inc t))))))))

(defn- type-in [txt] (swap! state assoc :code txt))
(defn- type-and-eval [txt]
  (swap! state assoc :code txt)
  (evaluate))
(defn- txt-in-stdout [reg]
  (wait-for #(re-find reg (:stdout @state))))
(defn- change-stdout []
  (let [old (:stdout @state)]
    (wait-for #(and (not= old (:stdout @state))
                    (:stdout @state)))))
(defn- change-stderr []
  (let [old (:stderr @state)]
    (wait-for #(and (not= old (:stderr @state))
                    (:stderr @state)))))

(defn- txt-for-selector [sel]
  (str (some-> js/document
               (.querySelector sel)
               .-innerText
               .trim)))
(defn- change-result []
  (let [old (txt-for-selector "#result")]
    (wait-for #(and (not= old (txt-for-selector "#result"))
                    (txt-for-selector "#result")))))

(defn- click-selector [sel]
  (-> js/document (.querySelector sel) .click))

(set! cards/test-timeout 8000)
(cards/deftest repl-evaluation
  (async done
    (async/go
     (connect!)
     (async/<! (wait-for #(-> @state :repls :eval)))

     (testing "evaluation works"
       (type-and-eval "(+ 2 3)")
       (check (async/<! (txt-in-stdout #"=> 5")) => "=> 5")
       (check (txt-for-selector "#result") => "5"))

     (testing "captures STDOUT"
       (type-and-eval "(println :FOOBAR)")
       (check (async/<! (change-stdout)) => #":FOOBAR"))

     (testing "captures STDERR"
       (type-and-eval "(.write *err* \"Error\")")
       (check (async/<! (change-stderr)) => #"Error"))

     (testing "detects NS on file"
       (type-and-eval "(do (ns clojure.string)\n(upper-case \"this is upper\"))")
       (check (async/<! (change-stdout)) => #"THIS IS UPPER"))

     (testing "evaluates and presents big lists"
       (ui/assert-out "( 0 1 2 3 4 5 6 7 8 9 ... )" "(range)")
       (ui/click-nth-link-and-assert
        "( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ... )" 2)
       (ui/click-nth-link-and-assert-children
        "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ..." 1)
       (testing "toggle off"
         (ui/click-nth-link-and-assert-children "" 1)))

     (testing "evaluates and presents big vectors"
       (ui/assert-out "[ 0 1 2 3 4 5 6 7 8 9 ... ]" "(vec (range 14))")
       (ui/click-nth-link-and-assert
        "[ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 ]" 2)
       (ui/click-nth-link-and-assert-children
        "0 1 2 3 4 5 6 7 8 9 10 11 12 13" 1))

     (testing "evaluates and presents big sets"
       (ui/assert-out "#{ 0 1 2 3 4 5 6 7 8 9 ... }" "(apply sorted-set (range 14))")
       (ui/click-nth-link-and-assert
        "#{ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 }" 2)
       (ui/click-nth-link-and-assert-children
        "0 1 2 3 4 5 6 7 8 9 10 11 12 13" 1))

     (testing "evaluates and presents maps"
       (ui/assert-out "{ :a ( 0 1 2 3 4 5 6 7 8 9 ... ) ,  :b 90 }"
                      "(sorted-map :a (range 12) :b 90)")
       (ui/click-nth-link-and-assert
        "{ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) ,  :b 90 }" 2)
       (ui/click-nth-link-and-assert-children
        "[ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) ] [ :b 90 ]" 1))

     (testing "evaluates and presents taggable objects"
       (ui/assert-out #"#.+Foo  \{ :a \( 0 1 2 3 4 5 6 7 8 9 ... \) ,  :b 20 \}"
                      "(do (defrecord Foo [a b]) (->Foo (range 15) 20))")
       (ui/click-nth-link-and-assert
        #"#.+Foo  \{ :a \( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 \) ,  :b 20 \}" 2)
       (ui/click-nth-link-and-assert-children
        "[ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 ) ] [ :b 20 ]" 1))

     (testing "evaluates and presents classes"
       (ui/assert-out "java.lang.Object ..."
                      "Object"))


     #_
     (testing "evaluates and presents taggable objects"
       (ui/assert-out #"java\.lang\.Object@[\da-f]+ \.\.\."
                      "(Object.)"))

     (disconnect!)
     (done))))

(defn main [])
(cards/start-devcard-ui!)

(txt-for-selector "#result .children .row:nth-child(2)")
