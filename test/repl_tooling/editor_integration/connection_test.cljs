(ns repl-tooling.editor-integration.connection-test
  (:require [clojure.test :refer [testing]]
            [check.core :refer [check]]
            [check.async-old :as a]
            [clojure.core.async :as async]
            [repl-tooling.editor-integration.connection :as connection]
            [repl-tooling.editor-helpers :as editor-helpers]
            [repl-tooling.eval :as eval]))

(a/def-async-test "Connecting to Clojure's REPL" {:teardown (connection/disconnect!)}
  (let [repls (async/promise-chan)
        stdout (async/promise-chan)
        stderr (async/promise-chan)
        result (async/promise-chan)
        error (async/promise-chan)
        disconnect (async/promise-chan)]
    (. (connection/connect-unrepl! "localhost" 2233
                                   #(async/put! stdout %)
                                   #(async/put! stderr %)
                                   #(cond
                                      (:result %) (async/put! result (:result %))
                                      (:error %) (async/put! error (:error %)))
                                   #(async/put! disconnect :DONE))
      then #(async/put! repls %))

    (testing "capturing result"
      (-> repls a/await! :clj/repl (eval/evaluate "(/ 10 2)" {} identity))
      (check (a/await! result) => 5))

    (testing "evaluating request-response with invalid EDN"
      (let [res (async/promise-chan)]
        (-> repls a/await! :clj/repl (eval/evaluate "{(keyword \"foo bar\") 10}" {}
                                                    #(async/put! res (editor-helpers/parse-result %))))
        (check (:result (a/await! res)) => {(keyword "foo bar") 10}))

      (let [res (async/promise-chan)]
        (-> repls a/await! :clj/repl (eval/evaluate "{(symbol \"foo bar\") 10}" {}
                                                    #(async/put! res (editor-helpers/parse-result %))))
        (check (:result (a/await! res)) => {(symbol "foo bar") 10})))

    (testing "capturing stdout"
      (-> repls a/await! :clj/repl (eval/evaluate '(prn :foo) {} identity))
      (check (a/await! stdout) => ":foo\n"))

    (testing "capturing stderr"
      (-> repls a/await! :clj/repl (eval/evaluate "(binding [*out* *err*] (prn :bar))" {} identity))
      (check (a/await! stderr) => ":bar"))

    (testing "capturing error"
      (-> repls a/await! :clj/repl (eval/evaluate "(/ 10 0)" {} identity))
      (check (a/await! error) => map?))

    (testing "disconnecting"
      (connection/disconnect!)
      (check (a/await! disconnect) => :DONE))))

(a/def-async-test "Batches of commands" {:teardown (connection/disconnect!)}
  (let [repls (async/promise-chan)
        stdout (async/chan 60000)]
    (. (connection/connect-unrepl! "localhost" 2233
                                   #(async/put! stdout %)
                                   #()
                                   #()
                                   #())
      then #(async/put! repls %))
    (-> repls a/await! :clj/repl (eval/evaluate "(doseq [n (range 2000)] (prn n))" {} identity))
    (doseq [n (range 2000)]
      (check (a/await! stdout) => (str n "\n")))))
