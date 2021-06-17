(ns repl-tooling.editor-integration.configs-test
  (:require [repl-tooling.editor-integration.configs :as configs]
            [clojure.test :refer [testing]]
            [devcards.core :as cards :include-macros true]
            [check.core :refer [check]]
            [repl-tooling.integration.fake-editor :as editor]
            [clojure.core.async :as async]
            [check.async-old :refer [async-test await!]]
            [repl-tooling.editor-integration.interpreter :as int]
            ["fs" :refer [writeFileSync]]))

(cards/defcard-rg fake-editor
  editor/editor
  editor/state)

(def ^:private config-file "/tmp/repl-tooling-test.clj")
(defn- change-config-file [txt]
  (writeFileSync config-file txt))

(def ^:private custom-commands (atom {}))

(cards/deftest config-eval
  (async-test "evaluating code"
    (testing "evaluates simple code"
      (check (int/evaluate-code {:code "(+ 1 2)" :editor-state (atom {})}) => 3))

    (testing "resolves promises with p/let"
      (-> (int/evaluate-code {:code "(p/let [a (promise 1) b (promise 2)] (+ a b))"
                              :editor-state (atom {})})
          await!
          (check => 3)))

    (testing "resolves mixed promises / non-promises"
      (-> (int/evaluate-code {:code "(p/let [a (promise 1) b 2] (+ a b))"
                              :editor-state (atom {})})
          await!
          (check => 3)))))

(cards/deftest custom-config
  (let [reg (async/chan)]
    (async-test "automatically registering custom commands"
      {:timeout 8000 :teardown (do
                                 (async/close! reg)
                                 (editor/disconnect!))}

      (change-config-file "(defn- qt [txt] (str \"'\" txt))
                         (defn q [] (update {:code \"(+ 1 2)\"} :code qt))")
      (editor/connect! {:config-file-path config-file
                        :register-commands (fn [cmds]
                                             (async/put! reg (-> cmds keys set))
                                             (reset! custom-commands cmds))})
      (testing "when connected, new commands are registered"
        (await! reg)
        (check @custom-commands => {:q {:command fn?}})
        (check ((-> @custom-commands :q :command) {:code "(+ 1 2)"}) => {:code "'(+ 1 2)"}))

      (testing "when changing the file, new commands are registered"
        (change-config-file "(defn p [args] :end)")
        (await! reg)
        (check (:q @custom-commands) => nil)
        (check @custom-commands => {:p {:command fn?}}))

      (testing "getting blocks"
        (editor/type "(range 3)")
        (change-config-file "(defn e-block [] (p/let [data (editor/get-top-block)]
          (editor/eval-and-render data)))")
        (await! reg)
        ((-> @custom-commands :e-block :command))
        (check (await! (editor/change-result)) => "(\n0\n \n1\n \n2\n)"))

      (testing "UNREPL reader tags (IncompleteStr)"
        (change-config-file "(defn long-str []
                              (p/let [res (editor/eval {:text (pr-str (apply str (range 2000)))})]
                               (prn (str \"a-\" (:result res)))))")
        (await! reg)
        ((-> @custom-commands :long-str :command))
        (check (await! (editor/change-stdout)) => #"a-01234567891011.*"))

      (testing "checking for errors"
        (editor/type "(range 4)")
        (change-config-file "(defn error [] (throw (ex-info \"Some-error\" {})))")
        (await! reg)
        ((-> @custom-commands :error :command))
        (check (await! (editor/change-result)) => #"Some-error")))))
