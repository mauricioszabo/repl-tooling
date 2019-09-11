(ns repl-tooling.editor-helpers-test
  (:require [clojure.test :refer-macros [testing]]
            [devcards.core :refer [deftest] :include-macros true]
            [check.core :refer-macros [check]]
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

(deftest text-in-rage
  (testing "gets text in same line"
    (check (helpers/text-in-range ":foo" [[0 1] [0 2]]) => "fo"))

  (testing "gets text in multiple lines"
    (check (helpers/text-in-range "21\n121\n2" [[0 1] [1 1]]) => "1\n12")))

(deftest toplevel-forms
  (testing "gets top-level forms"
    (check (helpers/top-levels simple-clj) =>
           [[[[0 0] [0 6]] "(+ 1 2)"]
            [[[0 8] [0 16]] "(+ (3) 4)"]
            [[[1 0] [2 1]] "[1 2\n3]"]]))

  (testing "gets top-level forms in complex CLJ code"
    (check (helpers/top-levels some-clj)
           => [[[[1 0] [1 10]] "(ns foobar)"]
               [[[3 0] [7 20]] "(defn foo [a b c]\n  (+ 1 2) ; ))\n\n (defn bar [x y z]\n   {:a x :b y :c z}))"]
               [[[9 0] [9 10]] "(ns barbaz)"]
               [[[11 0] [11 12]] "(def algo 10)"]]))

  (testing "get top-levels with syntax errors"
    (check (helpers/top-levels "(+ 1) ) (+ 2)")
           => [[[[0 0] [0 4]] "(+ 1)"]
               [[[0 8] [0 12]] "(+ 2)"]])))

(deftest getting-top-blocks
  (testing "text and range from top-block"
    (check (helpers/top-block-for some-clj [9 3])
           => [[[9 0] [9 10]] "(ns barbaz)"])
    (check (helpers/top-block-for simple-clj [0 8])
           => [[[0 8] [0 16]] "(+ (3) 4)"]))

  (testing "sets and other objects"
    (check (helpers/top-block-for "{:a 10 :b}\n()" [0 2]) => [[[0 0] [0 9]] "{:a 10 :b}"])
    (check (helpers/top-block-for "#{:a 10 :b}\n()" [0 2]) => [[[0 0] [0 10]] "#{:a 10 :b}"])
    (check (helpers/top-block-for "'(+ 1 2)\n()" [0 2]) => [[[0 0] [0 7]] "'(+ 1 2)"])
    (check (helpers/top-block-for "@(+ 1 2)\n()" [0 2]) => [[[0 0] [0 7]] "@(+ 1 2)"])
    (check (helpers/top-block-for "#(+ 1 2)\n()" [0 2]) => [[[0 0] [0 7]] "#(+ 1 2)"])
    (check (helpers/top-block-for "#?(:cljs 1)\n()" [0 2])
           => [[[0 0] [0 10]] "#?(:cljs 1)"])
    (check (helpers/top-block-for "{::some/key a}\n()" [0 2])
           => [[[0 0] [0 13]] "{::some/key a}"])
    (check (helpers/top-block-for "#_(+ 1 2)\n()" [0 2])
           => [[[0 2] [0 8]] "(+ 1 2)"])))

(deftest getting-blocks
  (testing "text and range from block"
    (check (helpers/block-for simple-clj [0 10]) => [[[0 8] [0 16]] "(+ (3) 4)"])
    (check (helpers/block-for simple-clj [1 2]) => [[[1 0] [2 1]] "[1 2\n3]"]))

  (testing "sets and other objects"
    (check (helpers/block-for "{:a 10 :b}\n()" [0 2]) => [[[0 0] [0 9]] "{:a 10 :b}"])
    (check (helpers/block-for "#{:a 10 :b}\n()" [0 2]) => [[[0 0] [0 10]] "#{:a 10 :b}"])
    (check (helpers/block-for "'(+ 1 2)\n()" [0 2]) => [[[0 0] [0 7]] "'(+ 1 2)"])
    (check (helpers/block-for "@(+ 1 2)\n()" [0 2]) => [[[0 0] [0 7]] "@(+ 1 2)"])
    (check (helpers/block-for "#(+ 1 2)\n()" [0 2]) => [[[0 0] [0 7]] "#(+ 1 2)"])
    (check (helpers/block-for "#?(:cljs 1)\n()" [0 2])
           => [[[0 0] [0 10]] "#?(:cljs 1)"])
    (check (helpers/block-for "{::some/key a}\n()" [0 2])
           => [[[0 0] [0 13]] "{::some/key a}"])
    (check (helpers/block-for "#_(+ 1 2)\n()" [0 2])
           => [[[0 2] [0 8]] "(+ 1 2)"]))

  (testing "invalid forms"
    (check (helpers/block-for "( ) 1 2)" [0 3]) => nil)
    (check (helpers/block-for ") (1 2)" [0 3]) => [[[0 2] [0 6]] "(1 2)"])))

(deftest getting-blocks-with-special-symbols
  (testing "top-block with syntax quote"
    (check (helpers/top-block-for "(defmacro foo [] `(+ 1 2))" [0 21])
           => [[[0 0] [0 25]] "(defmacro foo [] `(+ 1 2))"]))

  (testing "top-block with tags"
    (check (helpers/top-block-for "(defmacro foo [] #js [1 2])" [0 21])
           => [[[0 0] [0 26]] "(defmacro foo [] #js [1 2])"])))


(def ns-code "(ns foobar)\n(def foo 10)\n(ns barbaz)\n(def wow 1)\n\n")
(deftest getting-ns
  (testing "getting NS top-level"
    (check (helpers/ns-range-for ns-code [1 2]) => [[[0 0] [0 10]] 'foobar])
    (check (helpers/ns-range-for ns-code [1 2]) => [[[0 0] [0 10]] 'foobar]))

  (testing "getting second NS in form"
    (check (helpers/ns-range-for ns-code [3 4]) => [[[2 0] [2 10]] 'barbaz])))
