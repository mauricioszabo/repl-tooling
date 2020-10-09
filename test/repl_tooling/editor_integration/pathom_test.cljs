(ns repl-tooling.editor-integration.pathom-test
  (:require [devcards.core :as cards]
            [repl-tooling.commands-to-repl.pathom :as pathom]
            [clojure.test]
            [promesa.core :as p]
            [repl-tooling.integration.fake-editor :as fake]
            [check.async :refer [async-test check testing]]
            [repl-tooling.repl-client.clojure :as clj]
            [repl-tooling.repl-client.shadow-ws :as shadow-ws]))

(def config (atom {:eval-mode :prefer-clj
                   :project-paths [(. js/process cwd)]}))

(def clj-repls {:repl/eval #(instance? clj/Evaluator %)
                :repl/aux #(instance? clj/Evaluator %)
                :repl/clj #(instance? clj/Evaluator %)})
(def cljs-repls {:repl/eval #(instance? shadow-ws/ShadowCLJS %)
                 :repl/aux #(instance? shadow-ws/ShadowCLJS %)
                 :repl/clj #(instance? clj/Evaluator %)})

(cards/deftest pathom-resolver-with-repl
  (async-test "pathom resolvers" {:teardown (fake/disconnect!)}
    (fake/connect! {:get-config #(deref config)})

    (testing "resolves editor data"
      (swap! fake/state assoc :filename "somefile.cljc" :range [[0 0] [0 0]])
      (fake/type "(ns foo)\n\n(str 1 2)")
      (check (pathom/eql (-> @fake/state :editor-state)
                         [:editor/contents :editor/filename :editor/range])
             => {:editor/contents "(ns foo)\n\n(str 1 2)"
                 :editor/filename "somefile.cljc"
                 :editor/range [[0 0] [0 0]]}))

    (testing "derives information from the editor data"
      (swap! fake/state assoc :range [[2 1] [2 1]])
      (check (pathom/eql (-> @fake/state :editor-state)
                         [:editor/namespace :editor/ns-range
                          :editor/current-var :editor/current-var-range])
             => {:editor/namespace "foo"
                 :editor/ns-range [[0 0] [0 7]]
                 :editor/current-var "str"
                 :editor/current-var-range [[2 1] [2 3]]}))

    (testing "will get Clojure REPLs info from config only"
      (swap! config assoc :eval-mode :clj)
      (check (pathom/eql (:editor-state @fake/state) [:repl/eval :repl/aux :repl/clj])
             => clj-repls))

    (testing "will get ClojureScript REPLs info from config only"
      (fake/run-command! :connect-embedded)
      (swap! config assoc :eval-mode :cljs)
      (check (pathom/eql (:editor-state @fake/state) [:repl/eval :repl/aux :repl/clj])
             => cljs-repls))

    (testing "will get Clojure REPL info from config + editor data"
      (swap! fake/state assoc :filename "somefile.cljc")
      (swap! config assoc :eval-mode :prefer-clj)
      (check (pathom/eql (:editor-state @fake/state) [:repl/eval :repl/aux :repl/clj])
             => clj-repls)

      (swap! config assoc :eval-mode :prefer-cljs)
      (swap! fake/state assoc :filename "somefile.clj")
      (check (pathom/eql (:editor-state @fake/state) [:repl/eval :repl/aux :repl/clj])
             => clj-repls))

    (testing "will get ClojureScript REPL info from config + editor data"
      (swap! fake/state assoc :filename "somefile.cljc")
      (swap! config assoc :eval-mode :prefer-cljs)
      (check (pathom/eql (:editor-state @fake/state) [:repl/eval :repl/aux :repl/clj])
             => cljs-repls)

      (swap! config assoc :eval-mode :prefer-clj)
      (swap! fake/state assoc :filename "somefile.cljs")
      (check (pathom/eql (:editor-state @fake/state) [:repl/eval :repl/aux :repl/clj])
             => cljs-repls))

    (testing "Full qualified name variables"
      (swap! fake/state assoc :range [[2 1] [2 1]])
      (swap! config assoc :eval-mode :clj)
      (check (pathom/eql (:editor-state @fake/state) [:var/fqn])
             => {:var/fqn 'clojure.core/str})

      (swap! config assoc :eval-mode :cljs)
      (check (pathom/eql (:editor-state @fake/state) [:var/fqn])
             => {:var/fqn 'cljs.core/str}))

    (testing "getting meta from Clojure vars"
      (swap! config assoc :eval-mode :clj)
      (swap! fake/state assoc
             :range [[2 1] [2 1]] :code "(ns promesa.core)\n\n(str 1 2)")
      (check (pathom/eql (:editor-state @fake/state) [:var/meta])
             => {:var/meta {:doc #"With no args, returns the empty string"}}))

    (testing "getting meta from ClojureScript vars"
      (swap! config assoc :eval-mode :cljs)
      (fake/type "(ns repl-tooling.integration.fixture-app)\n\n(p/deferred 1 2)")
      (swap! fake/state assoc :range [[2 1] [2 1]])
      (check (pathom/eql (:editor-state @fake/state) [:var/meta])
             => {:var/meta {:doc #"Creates an empty promise"}}))

    (testing "getting meta from ClojureScript macros"
      (swap! config assoc :eval-mode :cljs)
      (fake/type "(ns repl-tooling.integration.fixture-app)\n\n(p/let [] )")
      (swap! fake/state assoc :range [[2 1] [2 1]])
      (check (pathom/eql (:editor-state @fake/state) [:var/meta])
             => {:var/meta {:doc #"always returns promise"}}))))

    ; TODO: Test all namespaces
    ; TODO: Test all namespaces in CLJS
    ; TODO: Test all vars in NS
    ; TODO: Test all vars in NS in CLJS

(cards/defcard-rg fake-editor
  fake/editor
  fake/state)
