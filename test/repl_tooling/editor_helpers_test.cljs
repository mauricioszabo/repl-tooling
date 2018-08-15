(ns repl-tooling.editor-helpers-test
  (:require [clojure.test :refer-macros [deftest testing run-tests]]
            [check.core :refer-macros [check]]
            [repl-tooling.editor-helpers :as editor]))
            ; [check.async-cljs :refer-macros [def-async-test await!]]
            ; [cljs.core.async :refer [>!] :refer-macros [go] :as async]
            ; [repl-tooling.repl-client.protocols :as repl]))

(def simple-clj
  "(+ 1 2) (+ (3) 4)
[1 2
3]")

(def some-clj "
(ns foobar)

(defn foo [a b c]
  (+ 1 2 ; ))
))

 (defn bar [x y z]
   {:a x :b y :c z})

(ns barbaz)

(def algo 10)")

(deftest stripping-comments
  (testing "simple comments"
    (check (editor/strip-comments "(+ ; foobar)") => "(+ "))

  (testing "comments inside strings"
    (check (editor/strip-comments "\"foo;bar\"; foobar)") => "\"foo;bar\"")))

(deftest toplevel-forms
  (testing "gets top-level forms"
    (check (editor/top-levels simple-clj) => [[[0 0] [0 6]]
                                              [[0 8] [0 16]]
                                              [[1 0] [2 1]]]))

  (testing "gets "))

(run-tests)
