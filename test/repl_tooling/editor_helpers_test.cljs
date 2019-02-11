(ns repl-tooling.editor-helpers-test
  (:require [clojure.test :refer-macros [testing]]
            [devcards.core :refer [deftest] :include-macros true]
            [check.core :refer-macros [check]]
            [repl-tooling.editor-helpers :as editor]
            [repl-tooling.editor-helpers :as helpers]))

(deftest parsing-data
  (testing "parses taggable object"
    (let [tag (:result (helpers/parse-result {:result "#some/tag {:foo 10, :bar 20}"}))]
      (check (helpers/obj tag) => {:foo 10 :bar 20})
      (check (helpers/tag tag) => "#some/tag ")))

  (testing "parses UNREPL taggable objects"
    (let [tag (:result (helpers/parse-result {:result (str "#unrepl/object ["
                                                           "#unrepl.java/class foo.Bar"
                                                           "\"0x1599d728\" "
                                                           "\"2018-10-02T00:00:00.000Z\" "
                                                           "{"
                                                           ":pr-str \"#some/tag {:foo 10, :bar 20}\""
                                                           "}]")}))]
      (check (helpers/obj tag) => {:foo 10 :bar 20})
      (check (helpers/tag tag) => "#some/tag ")))

  (testing "avoid 'default printer' for JAVA objects"
    (let [tag (:result (helpers/parse-result {:result (str "#unrepl/object "
                                                           "[#unrepl.java/class [byte] "
                                                           "\"0xd6acdf0a\" "
                                                           "(70 79 79 66 65 82) "
                                                           "{:bean {{:repl-tooling/... nil} "
                                                           "{:repl-tooling/... (get-more :G__610840)}} "
                                                           ":pr-str \"#object[\\\"[B\\\" 0xd6acdf0a]\"}]")}))]
      (check (helpers/tag tag) => "#object ")
      (check (helpers/obj tag) =includes=> {:object-id "0xd6acdf0a"
                                            :repr '(70 79 79 66 65 82)
                                            {:repl-tooling/... nil}
                                            {:repl-tooling/... '(get-more :G__610840)}}))))

(def simple-clj
  "(+ 1 2) (+ (3) 4)
[1 2
3]")

(def some-clj "
(ns foobar)

(defn foo [a b c]
  (+ 1 2) ; ))

 (defn bar [x y z]
   {:a x :b y :c z}))

(ns barbaz)

(def algo 10)")

(deftest stripping-comments
  (testing "simple comments"
    (check (editor/strip-comments "(+ ; foobar)") => "(+ "))

  (testing "comments inside strings"
    (check (editor/strip-comments "\"foo;bar\"; foobar)") => "\"foo bar\"")))

(deftest toplevel-forms
  (testing "gets top-level forms"
    (check (map #(editor/text-in-range simple-clj %) (editor/top-levels simple-clj)) =>
           ["(+ 1 2)"
            "(+ (3) 4)"
            "[1 2\n3]"]))

  (testing "gets top-level forms in complex CLJ code"
    (check (editor/top-levels some-clj) => [[[1 0] [1 10]]
                                            [[3 0] [7 20]]
                                            [[9 0] [9 10]]
                                            [[11 0] [11 12]]])))

(def ns-code "(ns foobar)\n(def foo 10)\n(ns barbaz)\n(def wow 1)\n\n")
(deftest getting-ns
  (testing "getting NS top-level"
    (check (editor/ns-range-for ns-code [[1 2]]) => [[0 0] [0 10]])
    (check (editor/ns-range-for ns-code [[1 2]]) => [[0 0] [0 10]]))

  (testing "getting second NS in form"
    (check (editor/ns-range-for ns-code [[3 4]]) => [[2 0] [2 10]])
    (check (editor/ns-name-for ns-code [[3 4]]) => 'barbaz)))
