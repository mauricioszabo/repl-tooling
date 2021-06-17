(ns repl-tooling.features.shadow-cljs-test
  (:require [repl-tooling.features.shadow-cljs :as shadow]
            [check.core :refer [check]]
            [check.async-old :refer [await! async-test]]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.eval-helpers :as h]
            [repl-tooling.repl-client.shadow-ws :as shadow-ws]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.eval :as eval]
            [clojure.core.async :as async]
            [promesa.core :as p]
            [clojure.test]
            [repl-tooling.features.promised-test :refer [promised-test testing]]
            ["fs" :refer [readFileSync]]))

(def port (-> ".shadow-cljs/http.port" readFileSync str js/parseInt delay))
(def token (-> ".shadow-cljs/server.token" readFileSync str delay))

(defn connect! []
  (p/let [output (atom [])
          repl (shadow-ws/connect! {:id :shadow
                                    :build-id :fixture
                                    :host "localhost"
                                    :port @port
                                    :token @token
                                    :on-output #(swap! output conj %)})]
    [repl output]))

(cards/deftest shadow-cljs-websocket
  (promised-test {:teardown (repls/disconnect! :shadow)}
    (p/let [[repl outputs] (connect!)]
      (testing "connecting to Shadow"
        (p/delay 100)

        (testing "evaluating results"
          (p/let [res (eval/eval repl "(+ 1 2)")]
            (check res => {:result 3 :as-text "3" :parsed? true})))

        (testing "evaluating to errors"
          (p/let [res (p/catch (eval/eval repl "(throw (ex-info :foo {}))")
                               (fn [error] error))]
            (check res => {:error map? :as-text #"#error.*:foo"})))

        (testing "evaluating syntax errors"
          (p/let [res (p/catch (eval/eval repl "(prn nonexistent-var)")
                               (fn [error] error))]
            (check res => {:error map? :as-text #"nonexistent-var"})))

        (testing "evaluating compile errors"
          (p/let [res (p/catch (eval/eval repl "(nil 10)")
                               (fn [error] error))]
            (check res => {:error map? :as-text #"Can't call nil"})))

        (testing "evaluating print commands"
          (p/let [res (eval/eval repl "(pr :SOME-TEXT)")]
            (check res => {:result nil})
            (check @outputs => [{:out ":SOME-TEXT"}])))

        (testing "evaluating custom shadow-cljs commands"
          (p/let [res (eval/eval repl
                                 (pr-str {:op :request-supported-ops, :to #{1}})
                                 {:shadow-command true})]
            (check res => {:result {:op :supported-ops
                                    :ops set?
                                    :from 1
                                    :call-id any?}})))))))

(cards/deftest shadow-cljs-wrong-build
  (promised-test {:teardown (repls/disconnect! :shadow-wrong-build)}
    (p/let [repl (shadow-ws/connect! {:id :shadow-wrong-build
                                      :build-id :wrong-id
                                      :host "localhost"
                                      :port @port
                                      :token @token})]

      (testing "connecting to Shadow"
        (p/delay 100)

        (testing "evaluating results"
          (p/let [res (p/catch (eval/eval repl "(+ 1 2)")
                               (fn [error] error))]
            (check res => {:error map? :as-text #":wrong-id"})))))))
