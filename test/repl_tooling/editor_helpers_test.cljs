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

(deftest toplevel-forms
  (check (:forms (editor/top-levels simple-clj))
         => [[[0 0] [0 6]]
             [[0 8] [0 16]]
             [[1 0] [2 1]]]))

(run-tests)
