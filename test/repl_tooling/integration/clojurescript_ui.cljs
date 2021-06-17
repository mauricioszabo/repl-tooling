(ns repl-tooling.integration.clojurescript-ui
  (:require [reagent.core :as r]
            [clojure.walk :as walk]
            [repl-tooling.editor-integration.connection :as conn]
            [repl-tooling.integration.ui-macros :as ui]
            [repl-tooling.editor-integration.renderer :as render]
            [repl-tooling.integration.fake-editor :as fake
             :refer [change-stdout txt-for-selector type-and-eval change-result]]
            [clojure.test :refer [is]]
            [check.async :refer [async-test check testing]]
            [promesa.core :as p]
            [clojure.core.async :as async]
            [devcards.core :as cards :include-macros true]))

(cards/defcard-rg fake-editor
  fake/editor
  fake/state)

(defn- click-selector [sel]
  (-> js/document (.querySelector sel) .click))

(set! cards/test-timeout 20000)

(cards/deftest repl-evaluation
  (async-test "ClojureScript REPL evaluation" {:teardown (fake/disconnect!)
                                               :timeout 8000}
    (fake/connect! {:notify prn})
    (swap! fake/state assoc :filename "foo.cljs")
    (fake/run-command! :connect-embedded)
    (p/delay 1000)

    (testing "evaluation works for nil"
      (fake/type-and-eval "nil")
      (check (fake/change-result-p) => "nil"))

    (testing "evaluation works, but doesn't print something on STDOUT"
      (fake/type-and-eval "(/ 10 0)")
      (check (fake/change-result-p) => "##Inf")
      (is (not (re-find #"=>" (fake/txt-for-selector "#stdout")))))

    (testing "evaluate blocks"
      (swap! fake/state assoc
             :code "(+ 1 2)\n\n(+ 2 \n  (+ 3 4))"
             :range [[3 3] [3 3]])
      (fake/run-command! :evaluate-block)
      (check (fake/change-result-p) => "7"))

    (testing "evaluate top blocks"
      (swap! fake/state assoc
             :code "(+ 1 2)\n\n(+ 2 \n  (+ 3 4))"
             :range [[3 3] [3 3]])
      (fake/run-command! :evaluate-top-block)
      (check (fake/change-result-p) => "9"))

    (testing "displays booleans"
      (ui/type-and-assert-result "true" "true")
      (ui/type-and-assert-result "false" "false")
      (ui/type-and-assert-result "nil" "nil"))

    (testing "captures STDOUT"
      (fake/type-and-eval "(println :FOOBAR)")
      (check (fake/change-stdout-p) => #":FOOBAR"))

    (testing "detects NS on file"
      (swap! fake/state assoc
             :code "(ns clojure.string)\n(upper-case \"this is upper\")"
             :range [[1 1] [1 1]])
      (fake/run-command! :evaluate-block)
      (check (fake/change-result-p) => #"THIS IS UPPER"))

    (testing "displays invalid EDN"
      (ui/type-and-assert-result "{ :foo bar 10 }" "{(keyword \"foo bar\") 10}")
      (ui/click-link-and-assert-children
       "[ :foo bar 10 ]" 1))

    ; TODO: All of these!
    ; (testing "evaluates and presents big strings"
    ;   (ui/assert-out (str "\"01234567891011121314151617181920212223242526272829"
    ;                       "303132333435363738394041424344 ... \"")
    ;                  "(apply str (range 100))")
    ;   (ui/click-link-and-assert
    ;    (str "\"0123456789101112131415161718192021222324252627282930313233343"
    ;         "536373839404142434445464748495051525354555657585960616263646566"
    ;         "676869707172737475767778798081828384 ... \"")
    ;    1))
    ;
    ; (testing "evaluates and presents big lists"
    ;   (ui/assert-out "( 0 1 2 3 4 5 6 7 8 9 ... )" "(range)")
    ;   (ui/click-link-and-assert
    ;    "( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ... )" 2)
    ;   (ui/click-link-and-assert-children
    ;    "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ..." 1)
    ;   (testing "toggle off"
    ;     (ui/click-link-and-assert-children "" 1)))
    ;
    ; (testing "evaluates and presents big vectors"
    ;   (ui/assert-out "[ 0 1 2 3 4 5 6 7 8 9 ... ]" "(vec (range 14))")
    ;   (ui/click-link-and-assert
    ;    "[ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 ]" 2)
    ;   (ui/click-link-and-assert-children
    ;    "0 1 2 3 4 5 6 7 8 9 10 11 12 13" 1))
    ;
    ; (testing "evaluates and presents big sets"
    ;   (ui/assert-out "#{ 0 1 2 3 4 5 6 7 8 9 ... }" "(apply sorted-set (range 14))")
    ;   (ui/click-link-and-assert
    ;    "#{ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 }" 2)
    ;   (ui/click-link-and-assert-children
    ;    "0 1 2 3 4 5 6 7 8 9 10 11 12 13" 1))
    ;
    ; (testing "evaluates and presents maps"
    ;   (ui/assert-out "{ :a ( 0 1 2 3 4 5 6 7 8 9 ... ) , :b 90 }"
    ;                  "(sorted-map :a (range 12) :b 90)")
    ;   (ui/click-link-and-assert
    ;    "{ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) , :b 90 }" 2)
    ;   (ui/click-link-and-assert-children
    ;    "[ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) ] [ :b 90 ]" 1))
    ;
    (testing "evaluates and presents taggable objects"
      (ui/type-and-assert-result #"#.+Foo \{ :a \( 0 1 2 3 4 5 6 7 8 9 \) , :b 20 \}"
                                 "(do (defrecord Foo [a b]) (->Foo (range 10) 20))")
      #_
      (ui/click-link-and-assert-children
       "{ :a ( 0 1 2 3 4 5 6 7 8 9 ) , :b 20 }" 1))))

    ; (testing "evaluates promises, and patches result"
    ;   (ui/assert-out #"#promise <pending>"
    ;                  "(js/Promise. (fn [resolve] (js/setTimeout #(resolve 10) 200)))")
    ;   (let [res (async/<! (change-result))]
    ;     (check res => #"10")))))
      ; #_
      ; (ui/click-link-and-assert-children
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
