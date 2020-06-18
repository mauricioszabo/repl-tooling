(ns repl-tooling.features.shadow-cljs-test
  (:require [repl-tooling.features.shadow-cljs :as shadow]
            [check.core :refer [check]]
            [check.async :refer [await! async-test]]
            [devcards.core :as cards :include-macros true]
            ; [clojure.test :refer [async ]]
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
  (prn :WILL-CONNECT! (gensym))
  (shadow-ws/connect! :shadow :fixture "localhost" @port @token))

#_
(p/let [repl (connect!)]
  (def repl repl))
#_
(eval/eval repl :FOO "20")

(cards/deftest shadow-cljs-websocket
  (promised-test {:teardown (repls/disconnect! :shadow)}
    (p/let [repl (connect!)]
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
          (println "\n\n\n**********")
          (p/let [res (p/catch (eval/eval repl "(prn nonexistent-var)")
                               (fn [error] error))]
            (check res => {:error map? :as-text #"nonexistent-var"})))))))
