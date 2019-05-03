(ns repl-tooling.editor-helpers-test
  (:require [clojure.test :refer-macros [testing]]
            [devcards.core :refer [deftest] :include-macros true]
            [check.core :refer-macros [check]]
            [repl-tooling.editor-helpers :as editor]
            [repl-tooling.editor-helpers :as helpers]))

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

(def clj-with-strings "
(str 10 (+ 20
  35) \"(+ 20
      999)\"
90)
")

(deftest stripping-comments
  (testing "simple comments"
    (check (editor/strip-comments "(+ ; foobar)") => "(+ "))

  (testing "comments inside strings"
    (check (editor/strip-comments "\"foo;bar\"; foobar)") => "\"foo bar\"")))

(deftest toplevel-forms
  (testing "gets top-level forms"
    (check (editor/top-levels simple-clj) =>
           [[[[0 0] [0 6]] "(+ 1 2)"]
            [[[0 8] [0 16]] "(+ (3) 4)"]
            [[[1 0] [2 1]] "[1 2\n3]"]]))


  (testing "gets top-level forms in complex CLJ code"
    (check (editor/top-levels some-clj)
           => [[[[1 0] [1 10]] "(ns foobar)"]
               [[[3 0] [7 20]] "(defn foo [a b c]\n  (+ 1 2) ; ))\n\n (defn bar [x y z]\n   {:a x :b y :c z}))"]
               [[[9 0] [9 10]] "(ns barbaz)"]
               [[[11 0] [11 12]] "(def algo 10)"]])))

(deftest getting-blocks
  (testing "text and range from top-block"
    (check (editor/top-block-for some-clj [9 3])
           => [[[9 0] [9 10]] "(ns barbaz)"])
    (check (editor/top-block-for simple-clj [0 8])
           => [[[0 8] [0 16]] "(+ (3) 4)"])))

(def ns-code "(ns foobar)\n(def foo 10)\n(ns barbaz)\n(def wow 1)\n\n")
(deftest getting-ns
  (testing "getting NS top-level"
    (check (editor/ns-range-for ns-code [1 2]) => [[[0 0] [0 10]] 'foobar])
    (check (editor/ns-range-for ns-code [1 2]) => [[[0 0] [0 10]] 'foobar]))

  (testing "getting second NS in form"
    (check (editor/ns-range-for ns-code [3 4]) => [[[2 0] [2 10]] 'barbaz])))
