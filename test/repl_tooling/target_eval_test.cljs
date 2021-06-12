(ns repl-tooling.target-eval-test
  (:require [check.async :refer [check async-test testing]]
            [clojure.test :refer [deftest run-tests] :as test]
            [matcher-combinators.matchers :as m]
            [repl-tooling.editor-integration.connection :as connection]
            [promesa.core :as p]))

(def filename (atom "foo.cljs"))
(def state (atom nil))
(defn reset-state []
  (reset! state {:editor-data {:contents "(+ 1 2)"
                               :filename @filename
                               :range [[0 0] [0 0]]}
                 :result (p/deferred)
                 :out []
                 :commands {}}))

(defn- set-editor-data [data]
  (swap! state update :editor-data merge data))

(defn- connect! []
  (reset-state)
  (connection/connect! "localhost" 4444
                       {:on-stdout #(swap! state update :out conj %)
                        :register-commands #(swap! state assoc :commands %)
                        :on-stderr #(swap! state update :out conj %)
                        :on-disconnect #(reset-state)
                        ; :on-start-eval #(inline/new-result %)
                        :on-eval #(p/resolve! (:result @state) %)
                        ; :on-copy on-copy!
                        :editor-data #(-> state deref :editor-data)
                        :open-editor #(p/resolve! (:result @state) {:open-editor %})
                        :notify prn}))

(defn- evaluate! [code]
  (set-editor-data {:contents code})
  (let [p (p/deferred)
        cmd (-> @state :commands :evaluate-top-block :command)]
    (swap! state assoc :result p)
    (cmd)
    p))

(deftest evaluation-test
  (async-test "evaluates code" {:teardown #(connection/disconnect!)
                                :timeout 10000}
    (testing "connects to a REPL"
      (p/let [c (connect!)]
        (check @c => {:editor/commands {:evaluate-top-block {:name "Evaluate Top Block"
                                                             :command fn?}}})))

    (testing "evaluates literal commands"
      (p/let [res (evaluate! "true")] (check res => {:result {:result true}}))
      (p/let [res (evaluate! "false")] (check res => {:result {:result false}}))
      (p/let [res (evaluate! "nil")] (check res => {:result {:result nil}})))

    (testing "evaluates numbers"
      (p/let [res (evaluate! "(+ 1 2)")] (check res => {:result {:result 3}}))
      (p/let [res (evaluate! "(/ 10 2.0)")] (check res => {:result {:result 5.0}}))
      (p/let [res (evaluate! "20")] (check res => {:result {:result 20}})))

    (testing "evaluates strings"
      (p/let [res (evaluate! "(str 1 2 3 4)")] (check res => {:result {:result "1234"}})))

    (testing "evaluates regexp"
      (p/let [res (evaluate! "#\"foo\"")]
        (check res => {:result {:result regexp?}})))

    (testing "evaluates keywords and symbols"
      (p/let [res (evaluate! "(keyword \"foo\")")]
        (check res => {:result {:result :foo}}))
      (p/let [res (evaluate! "(keyword \"foo bar\")")]
        (check res => {:result {:result (keyword "foo bar")}}))
      (p/let [res (evaluate! "(symbol \"foo\")")]
        (check res => {:result {:result 'foo}}))
      (p/let [res (evaluate! "(symbol \"foo bar\")")]
        (check res => {:result {:result (symbol "foo bar")}})))

    (testing "evaluates collections"
      (p/let [res (evaluate! "(list 1 2 3)")] (check res => {:result {:result '(1 2 3)}}))
      (p/let [res (evaluate! "[1 2 3]")] (check res => {:result {:result [1 2 3]}}))
      (p/let [res (evaluate! "#{1 2 3}")] (check res => {:result {:result #{1 2 3}}}))
      (p/let [res (evaluate! "{:a 1 :b 2}")]
        (check res => {:result {:result {:a 1 :b 2}}})))

    (testing "evaluates errors"
      (p/let [res (evaluate! "(throw (ex-info \"Foo\" {}))")]
        (check res => {:result {:error {:message "Foo"}}})))))

(defmethod test/report [::test/default :summary] [{:keys [test pass fail error]}]
  (println "Ran" test "tests containing" (+ pass fail error) "assertions.")
  (println pass "passed," fail "failures," error "errors.")
  (if (= 0 fail error)
    (js/process.exit 0)
    (js/process.exit 1)))

(defn run [file-name]
  (reset! filename file-name)
  (run-tests))

#_(reset! filename "foo.clj")
