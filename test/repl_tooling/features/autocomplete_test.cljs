(ns repl-tooling.features.autocomplete-test
  (:require [reagent.core :as r]
            [clojure.test :refer [async testing is] :include-macros true]
            [clojure.core.async :as async :include-macros true]
            [check.core :refer-macros [check]]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.repl-client.clojure :as clj]
            [repl-tooling.repl-client :as client]
            [repl-tooling.features.autocomplete.simple :as simple]
            [repl-tooling.features.autocomplete.compliment :as compliment]
            [repl-tooling.repl-helpers :as repl-helpers]))

(set! cards/test-timeout 8000)
(cards/deftest clojure-simple-autocomplete
  (async done
    (async/go
     (client/disconnect! :clj-simple)
     (let [repl (clj/repl :clj-simple "localhost" 2233 identity)
           chan (async/promise-chan)]

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
                                     {:candidate "str/replace-first" :type :function}])))

       (async/<! (async/timeout 500))
       (client/disconnect! :clj-simple)
       (done)))))

(cards/deftest clojurescript-simple-autocomplete
  (async done
    (async/go
     (client/disconnect! :cljs-simple)
     (let [repl (async/<! (repl-helpers/repl-for-cljs! :cljs-simple))
           chan (async/promise-chan)]

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
                                     {:candidate "st/replace-with" :type :function}])))

       (client/disconnect! :cljs-simple)
       (done)))))

(cards/deftest clojure-compliment-autocomplete
  (async done
    (async/go
     (client/disconnect! :clojure-compliment)
     (let [repl (clj/repl :clojure-compliment "localhost" 2233 identity)
           res (compliment/for-clojure repl 'user "(let [foo 10] fo)" "fo" 0 16)]
       (check (async/<! res) => [{:candidate "foo", :type :local}
                                 {:candidate "for", :type :macro, :ns "clojure.core"}
                                 {:candidate "force", :type :function, :ns "clojure.core"}
                                 {:candidate "format", :type :function, :ns "clojure.core"}])
       (client/disconnect! :clojure-compliment)
       (done)))))

(cards/deftest clojurescript-compliment-autocomplete
  (async done
    (async/go
     (client/disconnect! :cljs-compliment)
     (let [repl (clj/repl :cljs-compliment "localhost" 2233 identity)
           cljs-env '(shadow.cljs.devtools.api/compiler-env :fixture)]

        (testing "will complete local and NS variables"
          (let [res (compliment/for-cljs repl cljs-env 'cljs.user "(let [foo 10] fo)" "fo" 0 16)]
            (check (async/<! res) => [{:candidate "foo", :type :local}
                                      {:candidate "for", :type :macro, :ns "cljs.core"}
                                      {:candidate "force", :type :function, :ns "cljs.core"}])))

        (testing "will complete keyword"
          (let [res (compliment/for-cljs repl cljs-env 'cljs.user "" ":cljs-autocom" 0 16)]
            (check (async/<! res) => [{:candidate ":cljs-autocomplete-keyword", :type :keyword}])))

       (client/disconnect! :cljs-compliment)
       (done)))))
