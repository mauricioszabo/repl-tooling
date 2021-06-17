(ns repl-tooling.editor-helpers-test
  (:require [clojure.test :refer [testing] :as test]
            [devcards.core :as cards :include-macros true]
            [check.core :refer [check]]
            [check.mocks :refer [mocking]]
            [repl-tooling.editor-helpers :as helpers]
            ["fs" :as fs]))

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

(cards/deftest text-in-range
  (testing "gets text in same line"
    (check (helpers/text-in-range ":foo" [[0 1] [0 2]]) => "fo"))

  (testing "gets text in multiple lines"
    (check (helpers/text-in-range "21\n121\n2" [[0 1] [1 1]]) => "1\n12"))

  (testing "out of bounds"
    (check (helpers/text-in-range "123\n456\n" [[0 0] [0 5]]) => "123")
    (check (helpers/text-in-range "123\n456\n" [[0 0] [1 5]]) => "123\n456")
    (check (helpers/text-in-range "123\n456\n" [[0 0] [3 5]]) => "123\n456\n")
    (check (helpers/text-in-range "123\n456\n" [[1 0] [4 5]]) => "456\n")))

(cards/deftest toplevel-forms
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

(cards/deftest getting-top-blocks
  (testing "text and range from top-block"
    (check (helpers/top-block-for some-clj [9 3])
           => [[[9 0] [9 10]] "(ns barbaz)"])
    (check (helpers/top-block-for simple-clj [0 8])
           => [[[0 8] [0 16]] "(+ (3) 4)"]))

  (testing "text and range from end of line"
    (check (helpers/top-block-for simple-clj [0 17])
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

(cards/deftest getting-blocks
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
    (check (helpers/block-for "#_(+ 1 2)\n()" [0 3])
           => [[[0 2] [0 8]] "(+ 1 2)"]))

  (testing "invalid forms"
    (check (helpers/block-for "( ) 1 2)" [0 4]) => nil)
    (check (helpers/block-for ") (1 2)" [0 3]) => [[[0 2] [0 6]] "(1 2)"]))

  (testing "extreme cases"
    (check (helpers/block-for "(+ (- 1 2) 3)" [0 0]) =>
           [[[0 0] [0 12]] "(+ (- 1 2) 3)"])
    (check (helpers/block-for "(+ (- 1 2) 3)" [0 3]) =>
           [[[0 3] [0 9]] "(- 1 2)"])
    (check (helpers/block-for "(+ (- 1 2) 3)" [0 13]) =>
           [[[0 0] [0 12]] "(+ (- 1 2) 3)"])
    (check (helpers/block-for "(+ (- 1 2) 3)" [0 10]) =>
           [[[0 3] [0 9]] "(- 1 2)"])))

(cards/deftest getting-blocks-with-special-symbols
  (testing "top-block with syntax quote"
    (check (helpers/top-block-for "(defmacro foo [] `(+ 1 2))" [0 21])
           => [[[0 0] [0 25]] "(defmacro foo [] `(+ 1 2))"]))

  (testing "top-block with tags"
    (check (helpers/top-block-for "(defmacro foo [] #js [1 2])" [0 21])
           => [[[0 0] [0 26]] "(defmacro foo [] #js [1 2])"])))


(def ns-code "(ns foobar)\n(def foo 10)\n(ns barbaz)\n(def wow 1)\n\n")
(cards/deftest getting-ns
  (testing "getting NS top-level"
    (check (helpers/ns-range-for ns-code [1 2]) => [[[0 0] [0 10]] 'foobar])
    (check (helpers/ns-range-for ns-code [1 2]) => [[[0 0] [0 10]] 'foobar]))

  (testing "finds next NS if cursor is before it"
    (check (helpers/ns-range-for (str "\n" ns-code) [0 0]) => [[[1 0] [1 10]] 'foobar]))

  (testing "getting second NS in form"
    (check (helpers/ns-range-for ns-code [3 4]) => [[[2 0] [2 10]] 'barbaz]))

  (testing "namespace declaration with complex metadata"
    (let [txt "(ns ^{:config '{:some-keyword some-symbol}} my-ns)\n\n"]
      (check (helpers/ns-range-for txt [2 0])
             => [[[0 0] [0 49]] 'my-ns]))))

(cards/deftest getting-current-var
  (testing "getting var under cursor"
    (check (helpers/current-var " some-var " [0 1]) => [[[0 1] [0 8]] "some-var"]))

  (testing "getting var on the end of cursor"
    (check (helpers/current-var " some-var " [0 9]) => [[[0 1] [0 8]] "some-var"])))

(cards/deftest port-detection
  (testing "don't return anything if there's no file"
    (mocking
     (fs/existsSync "/tmp/.socket-repl-port") => false
     (fs/existsSync "/tmp/.shadow-cljs/socket-repl.port") => false
     (fs/existsSync "/tmp/.nrepl-port") => false
     ---
     (check (helpers/get-possible-port ["/tmp"] true nil) => nil)))

  (testing "return shadow-cljs port first"
    (mocking
     (fs/existsSync "/tmp/.socket-repl-port") => false
     (fs/existsSync "/tmp2/.socket-repl-port") => true
     (fs/readFileSync "/tmp2/.socket-repl-port") => "9999"
     (fs/existsSync "/tmp/.shadow-cljs/socket-repl.port") => true
     (fs/existsSync "/tmp/.nrepl-port") => true
     ---
     (check (helpers/get-possible-port ["/tmp" "/tmp2"] true nil) => 9999)))

  (testing "return socket-repl port first"
    (mocking
     (fs/existsSync "/tmp/.socket-repl-port") => false
     (fs/existsSync "/tmp2/.socket-repl-port") => true
     (fs/readFileSync "/tmp2/.socket-repl-port") => "5555"
     (fs/existsSync "/tmp/.shadow-cljs/socket-repl.port") => true
     (fs/existsSync "/tmp/.nrepl-port") => true
     ---
     (check (helpers/get-possible-port ["/tmp" "/tmp2"] true nil) => 5555)))

  (testing "return shadow-cljs socket repl port second"
    (mocking
     (fs/existsSync "/tmp/.socket-repl-port") => false
     (fs/existsSync "/tmp2/.socket-repl-port") => false
     (fs/existsSync "/tmp/.shadow-cljs/socket-repl.port") => "12112"
     (fs/readFileSync "/tmp/.shadow-cljs/socket-repl.port") => "12112"
     (fs/existsSync "/tmp/.nrepl-port") => true
     ---
     (check (helpers/get-possible-port ["/tmp" "/tmp2"] true nil) => 12112)))

  (testing "return nREPL port if the config is active"
    (mocking
     (fs/existsSync "/tmp/.socket-repl-port") => false
     (fs/existsSync "/tmp2/.socket-repl-port") => false
     (fs/existsSync "/tmp/.shadow-cljs/socket-repl.port") => false
     (fs/existsSync "/tmp2/.shadow-cljs/socket-repl.port") => false
     (fs/existsSync "/tmp/.nrepl-port") => true
     (fs/readFileSync "/tmp/.nrepl-port") => "1020"
     ---
     (check (helpers/get-possible-port ["/tmp" "/tmp2"] true nil) => 1020)
     (check (helpers/get-possible-port ["/tmp" "/tmp2"] false nil) => nil)))

  (testing "return the original port if one is passed"
    (mocking
     (fs/existsSync "/tmp/.socket-repl-port") => true
     (fs/existsSync "/tmp2/.socket-repl-port") => true
     (fs/existsSync "/tmp/.shadow-cljs/socket-repl.port") => true
     (fs/existsSync "/tmp2/.shadow-cljs/socket-repl.port") => true
     (fs/existsSync "/tmp/.nrepl-port") => true
     (fs/existsSync "/tmp2/.nrepl-port") => true
     ---
     (check (helpers/get-possible-port ["/tmp" "/tmp2"] true 2910) => 2910))))
