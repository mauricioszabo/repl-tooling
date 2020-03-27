(ns repl-tooling.integration.ui
  (:require [clojure.walk :as walk]
            [repl-tooling.integration.fake-editor :refer [editor evaluate state connect!]]
            [repl-tooling.integration.ui-macros :as ui :include-macros true]
            [clojure.test :refer [async testing is] :include-macros true]
            [check.core :refer-macros [check]]
            [clojure.core.async :as async :include-macros true]
            [devcards.core :as cards :include-macros true]
            [clojure.string :as str]
            [repl-tooling.editor-integration.connection :as conn]
            [schema.core :as s]

            [repl-tooling.editor-helpers-test]
            [repl-tooling.repl-client.parsing-test]
            [repl-tooling.repl-client.textual-representation-test]
            [repl-tooling.integration.clojurescript-ui]
            [repl-tooling.repl-client.evaluation-test]
            [repl-tooling.features.definition-test]
            [repl-tooling.features.autocomplete-test]
            [repl-tooling.editor-integration.autocomplete-test]
            [repl-tooling.repl-client.connection-test]
            [repl-tooling.integration.rendered-actions]
            [repl-tooling.editor-integration.interactive-test]
            [repl-tooling.editor-integration.doc-test]
            [repl-tooling.nrepl.nrepl-test]))

(cards/defcard-rg rendered-result
  (fn [result]
    [:div
     (pr-str
      (walk/prewalk
       #(if (satisfies? IDeref %)
          (cond-> @% (map? @%) (dissoc :repl))
          %)
       @result))])
  (:eval-result @state)
  {:watch-atom true})

(cards/defcard-rg fake-editor
  editor
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
  (some-> js/document (.querySelector sel) .click))

(defn click-chevron [n]
  (when-let [elem (aget (.. js/document (querySelectorAll "a.chevron")) n)]
    (.click elem)
    elem))

(set! cards/test-timeout 8000)
(cards/deftest repl-evaluation
  (async done
    (async/go
     (connect!)
     (async/<! (wait-for #(-> @state :repls :eval)))

     (testing "evaluation works"
       (type-and-eval "(+ 2 3)")
       (check (async/<! (txt-in-stdout #"=> 5")) =expect=> "=> 5")
       (check (txt-for-selector "#result") =expect=> "5"))

     (testing "evaluate blocks"
       (swap! state assoc
              :code "(+ 1 2)\n\n(+ 2 \n  (+ 3 4))"
              :range [[3 3] [3 3]])
       ((-> @state :commands :evaluate-block :command))
       (async/<! (change-stdout))
       (check (txt-for-selector "#result") =expect=> "7"))

     (testing "evaluate top blocks"
       (swap! state assoc
              :code "(+ 1 2)\n\n(+ 2 \n  (+ 3 4))"
              :range [[3 3] [3 3]])
       ((-> @state :commands :evaluate-top-block :command))
       (async/<! (change-stdout))
       (check (txt-for-selector "#result") =expect=> "9"))

     (testing "displays booleans"
       (ui/assert-out "true" "true")
       (ui/assert-out "false" "false")
       (ui/assert-out "nil" "nil"))

     (testing "displays UUIDs"
       (ui/assert-out "#uuid \"00000000-0000-0000-0000-000000000000\""
                      "(java.util.UUID/fromString \"00000000-0000-0000-0000-000000000000\")"))

     (testing "captures STDOUT"
       (type-and-eval "(println :FOOBAR)")
       (check (async/<! (change-stdout)) =expect=> #":FOOBAR"))

     (testing "captures STDERR"
       (type-and-eval "(.write *err* \"Error\")")
       (check (async/<! (change-stderr)) =expect=> #"Error"))

     (testing "detects NS on file"
       (type-and-eval "(do (ns clojure.walk)\n(stringify-keys {:foo 10}))")
       (check (async/<! (change-stdout)) =expect=> #"\"foo\" 10"))

     (testing "evaluates and presents big strings"
       (ui/assert-out (str "\"01234567891011121314151617181920212223242526272829"
                           "303132333435363738394041424344 ... \"")
                      "(apply str (range 100))")
       (ui/click-nth-link-and-assert
        (str "\"0123456789101112131415161718192021222324252627282930313233343"
             "536373839404142434445464748495051525354555657585960616263646566"
             "676869707172737475767778798081828384 ... \"")
        1))

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
       (ui/assert-out "{ :a ( 0 1 2 3 4 5 6 7 8 9 ... ) , :b 90 }"
                      "(sorted-map :a (range 12) :b 90)")
       (ui/click-nth-link-and-assert
        "{ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) , :b 90 }" 2)
       (ui/click-nth-link-and-assert-children
        "[ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 ) ] [ :b 90 ]" 1))

     (testing "evaluates and presents taggable objects"
       (ui/assert-out #"#.+Foo \{ :a \( 0 1 2 3 4 5 6 7 8 9 ... \) , :b 20 \}"
                      "(do (defrecord Foo [a b]) (->Foo (range 15) 20))")
       (ui/click-nth-link-and-assert
        #"#.+Foo \{ :a \( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 \) , :b 20 \}" 2))
       ; (ui/click-nth-link-and-assert-children
       ;  "[ :a ( 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 ) ] [ :b 20 ]" 1))

     (testing "evaluates and presents classes"
       (ui/assert-out "java.lang.Object ..."
                      "Object"))

     (testing "evaluates inner browseable structures"
       (ui/assert-out #"#foobar.baz/lolnein \.\.\."
                      "(->> (range 95 100)
     (map #(vector (symbol (apply str (range %)))
                   (tagged-literal 'foobar.baz/lolnein (doto (java.util.LinkedList.)
                                                             (.add %)
                                                             (.add %)))))
     (into {}))"))
     (click-chevron 0)
     (async/<! (change-result))

     (testing "map is too deep, we show just the ellision for object"
       (click-chevron 5)
       (async/<! (change-result))
       (check (str/replace (txt-for-selector "#result div:nth-child(5) div:nth-child(2) div.tagged")
                           #"(\n|\s+)+" " ")
              =expect=> #"#foobar.baz/lolnein \.\.\."))

     (testing "clicking the ellision for object should render its representation"
       (click-selector ".children .children div:nth-child(2) div div a")
       (wait-for #(click-chevron 6))
       (async/<! (change-result))
       (check (str/replace (txt-for-selector "#result .children div.tag:nth-child(2)")
                           #"(\n|\s+)+" " ")
              =expect=> #"\( 99 99 \)"))

     (testing "division by zero renders an exception"
       (ui/assert-out #"java.lang.ArithmeticException : \"Divide by zero\""
                      "(/ 10 0)"))

     (testing "shows exceptions on unidentified elements"
       (ui/assert-out #"Unable to resolve classname: SomeUnknownObject"
                      "(SomeUnknownObject.)"))

     (conn/disconnect!)
     (done))))

(cards/start-devcard-ui!)
(s/set-fn-validation! true)

(defn ^:dev/after-load main []
  (println "re-instrumenting"))
