(ns repl-tooling.features.autocomplete-test
  (:require [reagent.core :as r]
            [clojure.test :refer [async testing is]]
            [clojure.core.async :as async]
            [matcher-combinators.matchers :refer [embeds]]
            [check.core :refer [check]]
            [check.async-old :refer [await!]]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.features.autocomplete.simple :as simple]
            [repl-tooling.features.autocomplete.compliment :as compliment]
            [repl-tooling.features.autocomplete.suitable :as suit]
            [repl-tooling.eval-helpers :as h]))

(set! cards/test-timeout 20000)
(cards/deftest clojure-simple-autocomplete
  (h/async-with-repl "Clojure simple autocomplete"
    (testing "completing core functions"
      (let [res (simple/for-clj repl 'user "prn")]
        (check (await! res) => [{:candidate "prn" :type :function}
                                {:candidate "prn-str" :type :function}])))

    (testing "completing macros and private fns in current NS"
      (let [res (simple/for-clj repl 'repl-tooling.integration.ui-macros "type-and")]
        (check (await! res) => [{:candidate "type-and-assert-result" :type :function}
                                {:candidate "type-and-just-for-test" :type :function}
                                {:candidate "type-and-result" :type :function}])))

    (testing "completing imported vars"
      (let [res (simple/for-clj repl 'repl-tooling.integration.ui-macros "str/repl")]
        (check (await! res) => [{:candidate "str/replace" :type :function}
                                {:candidate "str/replace-first" :type :function}])))))

(cards/deftest clojurescript-simple-autocomplete
  (h/async-with-cljs-repl "ClojureScript with simple complete"
    (testing "completing core functions"
      (let [res (simple/for-cljs repl 'repl-tooling.integration.fixture-app "prn")]
        (check (await! res) => [{:candidate "prn" :type :function}
                                {:candidate "prn-str" :type :function}
                                {:candidate "prn-str-with-opts" :type :function}])))

    (testing "completing functions in current NS"
      (let [res (simple/for-cljs repl 'repl-tooling.integration.fixture-app "priva")]
        (check (await! res) => [{:candidate "private-fn" :type :function}
                                {:candidate "private-var" :type :function}]))
      (let [res (simple/for-cljs repl 'repl-tooling.integration.fixture-app "loc")]
        (check (await! res) => [{:candidate "local-fn" :type :function}
                                {:candidate "local-var" :type :function}])))

    (testing "completing imported vars"
      (let [res (simple/for-cljs repl 'repl-tooling.integration.fixture-app "st/repl")]
        (check (await! res) => [{:candidate "st/replace" :type :function}
                                {:candidate "st/replace-all" :type :function}
                                {:candidate "st/replace-first" :type :function}
                                {:candidate "st/replace-with" :type :function}])))))

(cards/deftest clojure-compliment-autocomplete
  (h/async-with-repl "Clojure with Compliment"
    (check (await! (compliment/for-clojure repl 'user "(let [foo 10] fo)" "fo" 0 16))
           => (embeds [{:candidate "foo", :type :local}
                       {:candidate "for", :type :macro, :ns "clojure.core"}
                       {:candidate "force", :type :function, :ns "clojure.core"}
                       {:candidate "format", :type :function, :ns "clojure.core"}]))))

(def cljs-env '(shadow.cljs.devtools.api/compiler-env :fixture))
(cards/deftest clojurescript-compliment-autocomplete
  (h/async-with-repl "ClojureScript with Compliment"
    (testing "will complete local and NS variables"
      (check (await! (compliment/for-cljs repl cljs-env 'cljs.user
                                          "(let [foo 10] fo)" "fo" 0 16))
             => (embeds [{:candidate "foo", :type :local}
                         {:candidate "for", :type :macro, :ns "cljs.core"}
                         {:candidate "force", :type :function, :ns "cljs.core"}])))

    (testing "will complete keyword"
      (check (await! (compliment/for-cljs repl cljs-env 'cljs.user ""
                                          ":cljs-autocom" 0 16))
             => [{:candidate ":cljs-autocomplete-keyword", :type :keyword}]))))

(cards/deftest clojurescript-suitable-autocomplete
  (h/async-with-cljs-repl "ClojureScript with Suitable"
    (testing "will complete JS objects"
      (check (await! (suit/for-cljs repl aux :fixture cljs-env 'cljs.user
                                    "(.. js/BigInt -len)" "-len" 0 18))
             => [{:candidate "-length", :type "var"}]))

    (testing "will complete NS variables"
      (check (await! (suit/for-cljs repl aux :fixture cljs-env 'cljs.user
                                    "(let [foo 10] fo)" "fo" 0 16))
             => (embeds [;{:candidate "foo", :type :local}
                         {:candidate "for", :type :macro, :ns "cljs.core"}
                         {:candidate "force", :type :function, :ns "cljs.core"}])))))
