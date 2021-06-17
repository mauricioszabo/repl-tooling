(ns repl-tooling.repl-client.textual-representation-test
  (:require [clojure.test :refer [testing is] :as test]
            [check.core :refer [check]]
            [clojure.core.async :as async]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.eval-helpers :refer [eval-on-repl eval-and-parse
                                               async-with-clj-repl]]
            [repl-tooling.editor-integration.renderer.protocols :as proto]
            [repl-tooling.editor-integration.renderer :as render]
            [repl-tooling.repl-client.clojure :as clj]))

(set! cards/test-timeout 20000)
(defn as-txt [parsed repl]
  (-> parsed (render/parse-result repl (atom {}))
      render/txt-for-result))

(cards/deftest evaluate-to-text
  (async-with-clj-repl "text repr"
    (testing "textual representation to pure text"
      (check (render/repr->lines [:row [:text "foobar"]]) => [["foobar"] {}])
      (check (render/repr->lines [:row [:text "foobar"] [:button "..." :f]])
             => [["foobar..."] {[0 6] :f [0 7] :f [0 8] :f}])

      (check (render/repr->lines [:row [:text "foobar"]
                                  [:row [:button "+" :e] [:text "Sub"]]])
             => [["foobar" "  +Sub"] {[1 2] :e}]))

    (testing "rendering leafs"
      (check (as-txt (eval-and-parse "10") repl) => [:row [:text "10"]]))

    (testing "rendering big strings"
      (let [after-more (async/promise-chan)
            parsed (render/parse-result (eval-and-parse "(apply str (range 80))") repl
                                        (atom {}))
            [root txt [_ txt2 more-fn] txt3] (render/txt-for-result parsed)
            big-str (str "\"01234567891011121314151617181920212223242526272829"
                         "303132333435363738394041424344")]
        (check root => :row)
        (check txt => [:text big-str])
        (check txt2 => "...")
        (check txt3 => [:text "\""])
        (check (proto/as-text @parsed parsed false) => [:text (str big-str "...\"")])

        (more-fn #(async/put! after-more parsed))
        (check (render/txt-for-result (async/<! after-more)) =>
               [:row [:text (str "\"01234567891011121314151617181920212223242526272"
                                 "82930313233343536373839404142434445464748495051"
                                 "525354555657585960616263646566676869707172737475"
                                 "76777879\"")]])))

    (testing "rendering simple vectors"
      (let [parsed (render/parse-result (eval-and-parse "[1 2]") repl (atom {}))
            [row expand text] (render/txt-for-result parsed)]
        (check row => :row)
        (check (take 2 expand) => [:expand "+"])
        (check text => [:text "[1 2]"])

        (testing "expanding"
          ((last expand) identity)
          (let [[row expand text row1 row2] (render/txt-for-result parsed)]
            (check row => :row)
            (check (take 2 expand) => [:expand "-"])
            (check text => [:text "[1 2]"])
            (check row1 => [:row [:text "1"]])
            (check row2 => [:row [:text "2"]])))))

    (testing "rendering nested vectors"
      (let [parsed (render/parse-result (eval-and-parse "[[1] [2]]") repl (atom {}))
            [row expand text] (render/txt-for-result parsed)]
        (check row => :row)
        (check (take 2 expand) => [:expand "+"])
        (check text => [:text "[[1] [2]]"])))

    (testing "rendering maps"
      (let [parsed (render/parse-result (eval-and-parse "{:foo 1 :bar 2}") repl (atom {}))
            [row expand text] (render/txt-for-result parsed)]
        (check row => :row)
        (check (take 2 expand) => [:expand "+"])
        (check text => [:text "{:foo 1, :bar 2}"])
        (testing "expanding"
          ((last expand) identity)
          (let [[row expand text [_ _ row1] [_ _ row2]] (render/txt-for-result parsed)]
            (check row => :row)
            (check (take 2 expand) => [:expand "-"])
            (check text => [:text "{:foo 1, :bar 2}"])
            (check row1 => [:text "[:foo 1]"])
            (check row2 => [:text "[:bar 2]"])))))

    (testing "rendering ellisions on lists"
      (let [parsed (render/parse-result (eval-and-parse "(range 20)") repl (atom {}))
            [row expand text ellision end] (render/txt-for-result parsed)]
        (check row => :row)
        (check text => [:text "(0 1 2 3 4 5 6 7 8 9 "])
        (check (take 2 ellision) => [:button "..."])
        (check end => [:text ")"])
        (testing "expanding"
          ((last expand) identity)
          (check (->> parsed render/txt-for-result last last (take 2)) =>
                 [:button "..."]))

        (testing "clicking on button"
          ((last expand) identity)
          (let [wait (async/promise-chan)
                more-fn (last ellision)]
            (more-fn #(async/put! wait :done))
            (async/<! wait)
           (check (last (render/txt-for-result parsed)) =>
                  [:text "(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)"])))))

    (testing "rendering ellisions on lists of lists"
      (let [parsed (render/parse-result (eval-and-parse "[(range)]") repl (atom {}))
            [txt] (render/repr->lines (render/txt-for-result parsed))]
        (check txt => ["+  [(0 1 2 3 4 5 6 7 8 9 ...)]"])))

    (testing "rendering simple exceptions"
      (let [parsed (render/parse-result (eval-and-parse "(/ 10 0)") repl (atom {}))
            [txt] (render/repr->lines (render/txt-for-result parsed))]
        (check (take 2 txt) => ["java.lang.ArithmeticException: \"Divide by zero\""
                                "  in clojure.lang.Numbers.divide (Numbers.java:188)"])))

    (testing "rendering exceptions with big text and data"
      (let [parsed (render/parse-result
                    (eval-and-parse "(throw (ex-info (apply str (range 100)) {:foo (range 100)}))")
                    repl
                    (atom {}))
            [txt] (render/repr->lines (render/txt-for-result parsed))]
        (check (take 2 txt) => [(str "clojure.lang.ExceptionInfo: \"012345678910111"
                                     "2131415161718192021222324252627282930313233"
                                     "3435363738394041424344...\"")
                                "  +  {:data {:foo (0 1 2 3 4 5 6 7 8 9 ...)}}"])))

    (testing "rendering tagged literals for non-collections"
      (let [parsed (render/parse-result
                    (eval-and-parse "(tagged-literal 'foobar :FOOBAR)")
                    repl
                    (atom {}))
            [txt] (render/repr->lines (render/txt-for-result parsed))]
        (check txt => ["+  #foobar :FOOBAR"])))

    (testing "rendering tagged literals"
      (let [parsed (render/parse-result
                    (eval-and-parse "(tagged-literal 'foobar {:foo :bar})")
                    repl
                    (atom {}))
            [txt funs] (render/repr->lines (render/txt-for-result parsed))]
        (check txt => ["+  #foobar {:foo :bar}"])

        ((get funs [0 0]) identity)
        (check (-> parsed render/txt-for-result render/repr->lines first)
               => ["-  #foobar {:foo :bar}"
                   "  +  {:foo :bar}"])))

    (testing "rendering browseable objects"
      (let [parsed (render/parse-result (eval-and-parse "Object") repl (atom {}))
            [txt funs] (render/repr->lines (render/txt-for-result parsed))
            wait (async/promise-chan)]
        (check txt => ["java.lang.Object..."])

        ((get funs [0 16]) #(async/put! wait :done))
        (async/<! wait)
        (-> parsed render/txt-for-result render/repr->lines first first
            (check => "java.lang.Object"))
        (-> parsed render/txt-for-result render/repr->lines first second
            (check => #"new java\.lang\.Object"))))

    (testing "rendering nested browseable objects"
      (let [parsed (render/parse-result (eval-and-parse "(list 1 (Object.))")
                                        repl
                                        (atom {}))
            [[txt] _] (render/repr->lines (render/txt-for-result parsed))]
        (check txt => #"\(1 \#object \[java\.lang\.Object.*\]\)")))

    #_
    (testing "rendering incomplete objects"
      (let [obj (helpers/->IncompleteObj (fn [ & args] ((last args) {:result ":FOO"})))
            parsed (render/parse-result {:result obj :parsed? true}
                                        repl
                                        (atom {}))
            [txt funs] (render/repr->lines (render/txt-for-result parsed))
            wait (async/promise-chan)]
        (check txt => ["..."])
        ((get funs [0 0]) #(async/put! wait :done))
        (async/<! wait)
        (-> parsed render/txt-for-result render/repr->lines first
            (check => [":FOO"]))))))
