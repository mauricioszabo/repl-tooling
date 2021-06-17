(ns repl-tooling.editor-integration.autocomplete-test
  (:require [reagent.core :as r]
            [clojure.test :refer [async testing is]]
            [clojure.core.async :as async]
            [check.core :refer [check]]
            [check.async-old :refer [async-test]]
            [matcher-combinators.matchers :refer [embeds]]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.repl-client.clojure :as clj]
            [repl-tooling.eval-helpers :as h]
            [repl-tooling.editor-integration.connection :as conn]))

(set! cards/test-timeout 20000)
(cards/deftest autodetection-of-autocomplete
  (async-test "autocomplete" {:teardown (conn/disconnect!)}
    (let [data (atom {:contents "(let [foa 10] foa)\n(let [foo 10] (+ fo))"
                      :filename "foo.clj"
                      :range [[1 19] [1 19]]})
          res (async/promise-chan)
          clj (async/promise-chan)
          cljs (async/promise-chan)
          _ (. (conn/connect! "localhost" 2233
                              {:editor-data #(deref data)
                               :prompt (constantly (.resolve js/Promise "fixture"))
                               :on-disconnect identity
                               :notify identity
                               :get-config
                               (constantly {:eval-mode :prefer-cljs
                                            :project-paths [(. js/process cwd)]})})
              then #(async/put! res %))
          {:keys [connect-embedded]} (:editor/commands @(async/<! res))
          {:keys [autocomplete]} (:editor/features @(async/<! res))]

      (testing "Clojure"
        (. (autocomplete) then #(async/put! clj %))
        (check (async/<! clj)
               => (embeds [{:candidate "foo", :type :local}
                           {:candidate "for", :type :macro, :ns "clojure.core"}
                           {:candidate "force", :type :function, :ns "clojure.core"}
                           {:candidate "format", :type :function, :ns "clojure.core"}])))

      (testing "ClojureScript"
        (swap! data assoc :filename "foo.cljs")
        (let [c (async/promise-chan)]
          (.then ((:command connect-embedded)) #(async/put! c %))
          (async/<! c))

        (. (autocomplete) then #(async/put! cljs %))
        (check (async/<! cljs) => (embeds [{:candidate "foo", :type :local}
                                           {:candidate "for", :type :macro, :ns "cljs.core"}
                                           {:candidate "force", :type :function, :ns "cljs.core"}]))))))
