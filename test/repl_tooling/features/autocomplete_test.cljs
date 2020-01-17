(ns repl-tooling.features.autocomplete-test
  (:require [reagent.core :as r]
            [clojure.test :refer [async testing is] :include-macros true]
            [clojure.core.async :as async :include-macros true]
            [check.core :refer-macros [check]]
            [check.async :refer-macros [await!]]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.features.autocomplete.simple :as simple]
            [repl-tooling.features.autocomplete.compliment :as compliment]
            [repl-tooling.repl-helpers :as repl-helpers]
            [repl-tooling.eval-helpers :include-macros true :as h]))
             ; :refer [eval-on-repl async-with-repl async-with-cljs-repl]]))

(set! cards/test-timeout 8000)
(cards/deftest clojure-simple-autocomplete
  (h/async-with-repl "Clojure simple autocomplete"
    (testing "completing core functions"
      (let [res (simple/for-clj repl 'repl-tooling.integration.fixture-app "prn")]
        (check (async/<! res) => [{:candidate "prn" :type :function}
                                  {:candidate "prn-str" :type :function}])))

    (testing "completing macros and private fns in current NS"
      (let [res (simple/for-clj repl 'repl-tooling.integration.ui-macros "type-and")]
        (check (async/<! res) => [{:candidate "type-and-just-for-test" :type :function}
                                  {:candidate "type-and-result" :type :function}])))

    (testing "completing imported vars"
      (let [res (simple/for-clj repl 'repl-tooling.integration.ui-macros "str/repl")]
        (check (async/<! res) => [{:candidate "str/replace" :type :function}
                                  {:candidate "str/replace-first" :type :function}])))))

(cards/deftest clojurescript-simple-autocomplete
  (h/async-with-cljs-repl "ClojureScript with simple complete"
    (testing "completing core functions"
      (let [res (simple/for-cljs repl 'repl-tooling.integration.fixture-app "prn")]
        (check (async/<! res) => [{:candidate "prn" :type :function}
                                  {:candidate "prn-str" :type :function}
                                  {:candidate "prn-str-with-opts" :type :function}])))

    (testing "completing functions in current NS"
      (let [res (simple/for-cljs repl 'repl-tooling.integration.fixture-app "priva")]
        (check (async/<! res) => [{:candidate "private-fn" :type :function}
                                  {:candidate "private-var" :type :function}]))
      (let [res (simple/for-cljs repl 'repl-tooling.integration.fixture-app "loc")]
        (check (async/<! res) => [{:candidate "local-fn" :type :function}
                                  {:candidate "local-var" :type :function}])))

    (testing "completing imported vars"
      (let [res (simple/for-cljs repl 'repl-tooling.integration.fixture-app "st/repl")]
        (check (async/<! res) => [{:candidate "st/replace" :type :function}
                                  {:candidate "st/replace-all" :type :function}
                                  {:candidate "st/replace-first" :type :function}
                                  {:candidate "st/replace-with" :type :function}])))))

(cards/deftest clojure-compliment-autocomplete
  (h/async-with-repl "Clojure with Compliment"
    (check (await! (compliment/for-clojure repl 'user "(let [foo 10] fo)" "fo" 0 16))
           => [{:candidate "foo", :type :local}
               {:candidate "for", :type :macro, :ns "clojure.core"}
               {:candidate "force", :type :function, :ns "clojure.core"}
               {:candidate "format", :type :function, :ns "clojure.core"}])))

(def cljs-env '(shadow.cljs.devtools.api/compiler-env :fixture))
(cards/deftest clojurescript-compliment-autocomplete
  (h/async-with-repl "ClojureScript with Compliment"
    (testing "will complete local and NS variables"
      (check (await! (compliment/for-cljs repl cljs-env 'cljs.user
                                          "(let [foo 10] fo)" "fo" 0 16))
             => [{:candidate "foo", :type :local}
                 {:candidate "for", :type :macro, :ns "cljs.core"}
                 {:candidate "force", :type :function, :ns "cljs.core"}]))

    (testing "will complete keyword"
      (check (await! (compliment/for-cljs repl cljs-env 'cljs.user ""
                                          ":cljs-autocom" 0 16))
             => [{:candidate ":cljs-autocomplete-keyword", :type :keyword}]))))
