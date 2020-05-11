(ns repl-tooling.editor-integration.configs-test
  (:require [repl-tooling.editor-integration.configs :as configs]
            [clojure.test :refer [testing]]
            [devcards.core :as cards]
            [check.core :refer [check]]
            [repl-tooling.integration.fake-editor :as editor]
            [clojure.core.async :as async]
            [check.async :refer [async-test await!]]
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
      (check (configs/evaluate-code "(+ 1 2)" (atom {})) => 3))

    (testing "resolves promises with let"
      (-> "(let [a (promise 1) b (promise 2)] (+ a b))"
          (configs/evaluate-code (atom {}))
          await!
          (check => 3)))

    (testing "resolves mixed promises / non-promises"
      (-> "(let [a (promise 1) b 2] (+ a b))"
          (configs/evaluate-code (atom {}))
          await!
          (check => 3)))))

(cards/deftest custom-config
  (let [reg (async/chan)]
    (async-test "automatically registering custom commands"
      {:timeout 8000 :teardown (do
                                 (async/close! reg)
                                 (editor/disconnect!))}

      (change-config-file "(defn- qt [txt] (str \"'\" txt))
                         (defn q [args] (update args :code qt))")
      (editor/connect! {:config-file-path config-file
                        :register-commands (fn [cmds]
                                             (async/put! reg :cmds)
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

      #_
      (testing "getting blocks"
        (editor/type "(range 3)")
        (change-config-file "(defn e-block [] (let [{:keys [text range]} (editor/top-block)]
          (eval-and-render text range)))")
        (await! reg)
        ((-> @custom-commands :e-block :command) {:code "(+ 1 2)"})
        (check (editor/change-result) =resolves=> "( 0 1 2 )")))))
