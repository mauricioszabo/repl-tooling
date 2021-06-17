(ns repl-tooling.integration.clojure-ui
  (:require [reagent.core :as r]
            [clojure.walk :as walk]
            [repl-tooling.editor-integration.connection :as conn]
            [repl-tooling.integration.ui-macros :as ui]
            [repl-tooling.editor-integration.renderer :as render]
            [repl-tooling.integration.fake-editor :as fake]
            [clojure.test :refer [is]]
            [check.async :refer [async-test check testing]]
            [promesa.core :as p]
            [clojure.string :as str]
            [devcards.core :as cards :include-macros true]))

(cards/defcard-rg fake-editor
  fake/editor
  fake/state)

(set! cards/test-timeout 20000)

(defn click-chevron [n]
  (when-let [elem (aget (.. js/document (querySelectorAll "a.chevron")) n)]
    (.click elem)
    elem))

(cards/deftest repl-evaluation
  (async-test "Clojure REPL evaluation" {:teardown (fake/disconnect!)
                                         :timeout 8000}
     (fake/connect! {:notify prn})

     (testing "evaluation works"
       (fake/type-and-eval "(+ 2 3)")
       (check (fake/change-result-p) => "5"))

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
       (check (fake/change-result-p) =expect=> "9"))

     (testing "displays booleans"
       (ui/type-and-assert-result "true" "true")
       (ui/type-and-assert-result "false" "false")
       (ui/type-and-assert-result "nil" "nil"))

     (testing "displays UUIDs"
       (ui/type-and-assert-result "#uuid \"00000000-0000-0000-0000-000000000000\""
                      "(java.util.UUID/fromString \"00000000-0000-0000-0000-000000000000\")"))

     (testing "captures STDOUT"
       (fake/type-and-eval "(println :FOOBAR)")
       (check (fake/change-stdout-p) =expect=> #":FOOBAR"))

     (testing "captures STDERR"
       (fake/type-and-eval "(.write *err* \"Error\")")
       (check (fake/change-stderr-p) =expect=> #"Error"))

     (testing "detects NS on file"
       (ui/type-and-assert-result #"\"foo\" 10"
                      "(do (ns clojure.walk)\n(stringify-keys {:foo 10}))"))

     (testing "evaluates and presents big strings"
       (ui/type-and-assert-result (str "\"01234567891011121314151617181920212223242526272829"
                                       "303132333435363738394041424344 ... \"")
                      "(apply str (range 100))")
       (ui/click-link-and-assert
        (str "\"0123456789101112131415161718192021222324252627282930313233343"
             "536373839404142434445464748495051525354555657585960616263646566"
             "676869707172737475767778798081828384 ... \"")
        1))

     (testing "evaluates and presents big lists"
       (ui/type-and-assert-result "( 0 1 2 3 4 5 6 7 8 9 ... )" "(range)")
       (ui/click-link-and-assert
        "( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ... )" 2)
       (ui/click-link-and-assert-children
        "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 ..." 1)
       (testing "toggle off"
         (ui/click-link-and-assert-children "" 1)))

     (testing "evaluates and presents big vectors"
       (ui/type-and-assert-result "[ 0 1 2 3 4 5 6 7 8 9 ... ]" "(vec (range 14))")
       (ui/click-link-and-assert
        "[ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 ]" 2)
       (ui/click-link-and-assert-children
        "0 1 2 3 4 5 6 7 8 9 10 11 12 13" 1))

     (testing "evaluates and presents big sets"
       (ui/type-and-assert-result "#{ 0 1 2 3 4 5 6 7 8 9 ... }" "(apply sorted-set (range 14))")
       (ui/click-link-and-assert
        "#{ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 }" 2)
       (ui/click-link-and-assert-children
        "0 1 2 3 4 5 6 7 8 9 10 11 12 13" 1))

     (testing "evaluates and presents maps"
       (ui/type-and-assert-result "{ :a ( 0 1 2 3 4 5 6 7 8 9 ... ) , :b 90 }"
                      "(sorted-map :a (range 12) :b 90)")
       (ui/click-link-and-assert
        "{ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) , :b 90 }" 2)
       (ui/click-link-and-assert-children
        "[ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) ] [ :b 90 ]" 1))

     (testing "evaluates and presents taggable objects"
       (ui/type-and-assert-result #"#.+Foo \{ :a \( 0 1 2 3 4 5 6 7 8 9 ... \) , :b 20 \}"
                      "(do (defrecord Foo [a b]) (->Foo (range 15) 20))")
       (ui/click-link-and-assert
        #"#.+Foo \{ :a \( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 \) , :b 20 \}" 2))
       ; (ui/click-link-and-assert-children
       ;  "[ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 ) ] [ :b 20 ]" 1))

     (testing "evaluates and presents classes"
       (ui/type-and-assert-result "java.lang.Object ..."
                      "(ns user) Object"))

     (testing "evaluates inner browseable structures"
       (ui/type-and-assert-result #"#foobar.baz/lolnein \.\.\."
                      "(->> (range 95 100)
     (map #(vector (symbol (apply str (range %)))
                   (tagged-literal 'foobar.baz/lolnein (doto (java.util.LinkedList.)
                                                             (.add %)
                                                             (.add %)))))
     (into {}))"))
     (click-chevron 0)
     (fake/change-result-p)

     (testing "map is too deep, we show just the ellision for object"
       (click-chevron 5)
       (fake/change-result-p)
       (check (str/replace (fake/txt-for-selector "#result div:nth-child(5) div:nth-child(2) div.tagged")
                           #"(\n|\s+)+" " ")
              =expect=> #"#foobar.baz/lolnein \.\.\."))

     (testing "clicking the ellision for object should render its representation"
       (some-> js/document
               (.querySelector ".children .children div:nth-child(2) div div a")
               .click)
       (fake/wait-for-p #(click-chevron 6))
       (fake/change-result-p)
       (check (str/replace (fake/txt-for-selector "#result .children div.tag:nth-child(2)")
                           #"(\n|\s+)+" " ")
              =expect=> #"\( 99 99 \)"))

    (testing "division by zero renders an exception"
      (ui/type-and-assert-result #"java.lang.ArithmeticException : \"Divide by zero\""
                     "(/ 10 0)"))

    (testing "shows exceptions on unidentified elements"
      (ui/type-and-assert-result #"Unable to resolve classname: SomeUnknownObject"
                     "(SomeUnknownObject.)"))))
