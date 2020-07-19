(ns repl-tooling.integration.clojurescript-ui
  (:require [reagent.core :as r]
            [clojure.walk :as walk]
            [repl-tooling.editor-integration.connection :as conn]
            [repl-tooling.integration.ui-macros :as ui]
            [repl-tooling.editor-integration.renderer :as render]
            [clojure.test :refer [async testing is]]
            [check.core :refer [check]]
            [clojure.core.async :as async]
            [devcards.core :as cards]))

(defonce state (r/atom {:host "localhost"
                        :port 2233
                        ; :code "(do (defrecord Foo [a b]) (->Foo (range 20) 20))"
                        :code "(/ 10 0)"
                        :commands nil
                        :stdout nil
                        :stderr nil
                        :range [[0 0] [0 0]]
                        :eval-result (r/atom nil)}))

(defn disconnect! []
  (conn/disconnect!))

(defn handle-disconnect []
  (reset! (:eval-result @state) nil)
  (swap! state assoc
         :stdout nil :stderr nil
         :commands nil))

(defn- eval-result [{:keys [result]}]
  (reset! (:eval-result @state)
          (render/parse-result result nil (atom {})))
  (swap! state update :stdout (fn [e] (str e "-> "
                                           (pr-str (or (:result result)
                                                       (:error result)))
                                           "\n"))))

(defn connect! []
  (when-not (-> @state :commands)
    (.. (conn/connect! (:host @state) (:port @state)
                       {:on-disconnect handle-disconnect
                        :on-stdout #(swap! state update :stdout (fn [e] (str e % "\n")))
                        ; :on-result prn
                        :get-rendered-results (fn [] [@(:eval-result @state)])
                        :on-stderr prn
                        :notify prn
                        :prompt (constantly (. js/Promise resolve "fixture"))
                        :get-config (constantly {:eval-mode :prefer-cljs
                                                 :project-paths [(. js/process cwd)]})
                        :editor-data #(let [code (:code @state)]
                                        {:contents code
                                         :filename "foo.cljs"
                                         :range (:range @state)})
                        :on-eval eval-result})
        (then (fn [st]
                (.. ((-> @st :editor/commands :connect-embedded :command))
                    (then #(if %
                             st
                             (prn :ERRRORRR!!!!!))))))
        (then (fn [st]
                (swap! state assoc
                       :commands (:editor/commands @st)
                       :range [[0 0] [0 0]]
                       :stdout "" :stderr ""))))))

(defn- evaluate []
  ((-> @state :commands :evaluate-top-block :command)))

(defn fake-editor [state]
  [:div
   [:h4 "Socket REPL connection"]
   [:p [:b "Hostname: "] [:input {:type "text" :value (:host @state)
                                  :on-change #(->> % .-target .-value (swap! state assoc :host))}]
       [:b " Port: "] [:input {:type "text" :value (:port @state)
                               :on-change #(->> % .-target .-value int (swap! state assoc :port))}]]
   [:textarea {:style {:width "100%" :height "100px"}
               :value (:code @state)
               :on-change #(->> % .-target .-value (swap! state assoc :code))}]
   [:p
    (if (-> @state :commands)
      [:span
       [:button {:on-click evaluate}
        "Evaluate"] " "
       [:button {:on-click disconnect!} "Disconnect!"]]
      [:button {:on-click connect!} "Connect!"])]
   [:p (if (-> @state :commands) "Connected" "Disconnected")]
   [:div
    (when-let [res @(:eval-result @state)]
      [:div
       [:h5 "RESULT"]
       [:pre
        [:div {:id "result" :class "result"}
         (render/view-for-result res)]]])]
   (when-let [out (:stdout @state)]
     [:div
      [:h5 "STDOUT"]
      [:pre {:id "stdout"} out]])
   (when-let [out (:stderr @state)]
     [:div
      [:h5 "STDERR"]
      [:pre out]])])

(cards/defcard-rg rendered-result
  (fn [result]
    [:div
     (pr-str
      (walk/prewalk
       #(if (satisfies? IDeref %)
          @%
          %)
       @result))])
  (:eval-result @state)
  {:watch-atom true})

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
  (swap! state assoc :code txt :range [[0 0] [0 0]])
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
      (async/<! (wait-for #(-> @state :commands)))
      (async/<! (async/timeout 1000))

      (testing "evaluation works for nil"
        (type-and-eval "nil")
        (async/<! (change-stdout))
        (check (txt-for-selector "#result") => "nil"))

      (testing "evaluation works, but doesn't print something on STDOUT"
        (type-and-eval "(+ 2 3)")
        (async/<! (change-stdout))
        (check (txt-for-selector "#result") => "5")
        (is (not (re-find #"=>" (txt-for-selector "#stdout")))))

      (testing "evaluate blocks"
        (swap! state assoc
               :code "(+ 1 2)\n\n(+ 2 \n  (+ 3 4))"
               :range [[3 3] [3 3]])
        ((-> @state :commands :evaluate-block :command))
        (async/<! (change-stdout))
        (check (txt-for-selector "#result") => "7"))

      (testing "evaluate top blocks"
        (swap! state assoc
               :code "(+ 1 2)\n\n(+ 2 \n  (+ 3 4))"
               :range [[3 3] [3 3]])
        ((-> @state :commands :evaluate-top-block :command))
        (async/<! (change-stdout))
        (check (txt-for-selector "#result") => "9"))

      (testing "displays booleans"
        (ui/assert-out "true" "true")
        (ui/assert-out "false" "false")
        (ui/assert-out "nil" "nil"))

      (testing "captures STDOUT"
        (type-and-eval "(println :FOOBAR)")
        (check (async/<! (change-stdout)) => #":FOOBAR"))

      (testing "detects NS on file"
        (swap! state assoc
               :code "(ns clojure.string)\n(upper-case \"this is upper\")"
               :range [[1 1] [1 1]])
        ((-> @state :commands :evaluate-block :command))
        (async/<! (change-stdout))
        (check (:stdout @state) => #"THIS IS UPPER"))

      (testing "displays invalid EDN"
        (ui/assert-out "{ :foo bar 10 }" "{(keyword \"foo bar\") 10}")
        (ui/click-nth-link-and-assert-children
         "[ :foo bar 10 ]" 1))

      ; TODO: All of these!
      ; (testing "evaluates and presents big strings"
      ;   (ui/assert-out (str "\"01234567891011121314151617181920212223242526272829"
      ;                       "303132333435363738394041424344 ... \"")
      ;                  "(apply str (range 100))")
      ;   (ui/click-nth-link-and-assert
      ;    (str "\"0123456789101112131415161718192021222324252627282930313233343"
      ;         "536373839404142434445464748495051525354555657585960616263646566"
      ;         "676869707172737475767778798081828384 ... \"")
      ;    1))
      ;
      ; (testing "evaluates and presents big lists"
      ;   (ui/assert-out "( 0 1 2 3 4 5 6 7 8 9 ... )" "(range)")
      ;   (ui/click-nth-link-and-assert
      ;    "( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ... )" 2)
      ;   (ui/click-nth-link-and-assert-children
      ;    "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ..." 1)
      ;   (testing "toggle off"
      ;     (ui/click-nth-link-and-assert-children "" 1)))
      ;
      ; (testing "evaluates and presents big vectors"
      ;   (ui/assert-out "[ 0 1 2 3 4 5 6 7 8 9 ... ]" "(vec (range 14))")
      ;   (ui/click-nth-link-and-assert
      ;    "[ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 ]" 2)
      ;   (ui/click-nth-link-and-assert-children
      ;    "0 1 2 3 4 5 6 7 8 9 10 11 12 13" 1))
      ;
      ; (testing "evaluates and presents big sets"
      ;   (ui/assert-out "#{ 0 1 2 3 4 5 6 7 8 9 ... }" "(apply sorted-set (range 14))")
      ;   (ui/click-nth-link-and-assert
      ;    "#{ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 }" 2)
      ;   (ui/click-nth-link-and-assert-children
      ;    "0 1 2 3 4 5 6 7 8 9 10 11 12 13" 1))
      ;
      ; (testing "evaluates and presents maps"
      ;   (ui/assert-out "{ :a ( 0 1 2 3 4 5 6 7 8 9 ... ) , :b 90 }"
      ;                  "(sorted-map :a (range 12) :b 90)")
      ;   (ui/click-nth-link-and-assert
      ;    "{ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) , :b 90 }" 2)
      ;   (ui/click-nth-link-and-assert-children
      ;    "[ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) ] [ :b 90 ]" 1))
      ;
      (testing "evaluates and presents taggable objects"
        (ui/assert-out #"#.+Foo \{ :a \( 0 1 2 3 4 5 6 7 8 9 \) , :b 20 \}"
                       "(do (defrecord Foo [a b]) (->Foo (range 10) 20))")
        #_
        (ui/click-nth-link-and-assert-children
         "{ :a ( 0 1 2 3 4 5 6 7 8 9 ) , :b 20 }" 1))

      (testing "evaluates promises, and patches result"
        (ui/assert-out #"#promise <pending>"
                       "(js/Promise. (fn [resolve] (js/setTimeout #(resolve 10) 200)))")
        (let [res (async/<! (change-result))]
          (check res => #"10")))
      ; #_
      ; (ui/click-nth-link-and-assert-children
      ;  "{ :a ( 0 1 2 3 4 5 6 7 8 9 ) , :b 20 }" 1))
      ;
      ; (testing "evaluates and presents classes"
      ;   (ui/assert-out "java.lang.Object ..."
      ;                  "Object"))
      ;
      ; (testing "evaluates inner browseable structures"
      ;   (ui/assert-out #"#foobar.baz/lolnein \.\.\."
      ;                  "(->> (range 95 100)
      ; (map #(vector (symbol (apply str (range %)))
      ;               (tagged-literal 'foobar.baz/lolnein (doto (java.util.LinkedList.)
      ;                                                         (.add %)
      ;                                                         (.add %)))))
      ; (into {}))"))
      ; (click-selector "#result a")
      ; (async/<! (change-result))
      ;
      ; (testing "map is too deep, we show just the ellision for object"
      ;   (click-selector ".children div:nth-child(5) a")
      ;   (async/<! (change-result))
      ;   (check (str/replace (txt-for-selector "#result .children") #"(\n|\s+)+" " ")
      ;          => #"#foobar.baz/lolnein \.\.\."))
      ;
      ; (testing "clicking the ellision for object should render its representation"
      ;   (click-selector ".children .children div:nth-child(2) a")
      ;   (async/<! (change-result))
      ;   (check (str/replace (txt-for-selector "#result .children") #"(\n|\s+)+" " ")
      ;          => #"#foobar.baz/lolnein \( 99 99 \) \.\.\."))

      (disconnect!)
      (done))))
