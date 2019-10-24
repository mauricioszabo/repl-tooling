(ns repl-tooling.repl-client.connection-test
  (:require [clojure.test :refer-macros [testing async is]]
            [devcards.core :as cards :include-macros true]
            [check.core :refer-macros [check]]
            [clojure.core.async :as async :include-macros true]
            [repl-tooling.repl-client.connection :as c]
            [reagent.core :as r]))

(set! cards/test-timeout 8000)
(defonce state (atom nil))
(defn- buffer [] (some-> @state :buffer deref))

(defn check-string [regexp]
  (async/go-loop [n 0]
    (when (< n 10)
      (let [s (peek (buffer))]
        (if (re-find regexp (str s))
          s
          (do
            (async/<! (async/timeout 100))
            (recur (inc n))))))))

(cards/deftest buffer-treatment
  (async done
    (let [buffer (atom [])
          lines (async/chan)
          frags (async/chan)]
      (async/go
        (c/treat-buffer! buffer #(async/put! lines (str %)) #(async/put! frags (str %)))

        (testing "emmits line"
          (swap! buffer conj "foo\n")
          (check (async/<! lines) => "foo")
          (check (async/<! frags) => "foo\n")
          (check @buffer => []))

        (testing "emmits complex line"
          (swap! buffer conj "foo")
          (swap! buffer conj "bar")
          (swap! buffer conj "b\nbaz")
          (check (async/<! lines) => "foobarb")
          (check @buffer => ["baz"]))

        (testing "emmits fragments"
          (check (async/<! frags) => "foobarb\n")
          (check (async/<! frags) => "baz")
          (check @buffer => []))

        (testing "emmits lines of already emitted frags"
          (swap! buffer conj "aar\n")
          (check (async/<! lines) => "bazaar")
          (check (async/<! frags) => "aar\n"))

        (testing "emmits nil when closed connection"
          (swap! buffer conj :closed)
          (check (async/<! frags) => "")
          (check (async/<! lines) => ""))

        (async/close! lines)
        (async/close! frags)
        (done)))))

(cards/deftest eval-cycle
  (async done
    (let [buffer (atom [])
          output (async/chan)
          results (async/chan)
          control (c/treat-buffer! buffer identity identity)]

      (async/go
       (c/prepare-evals control
                        #(async/put! output (or % :closed))
                        #(async/put! results (or % :closed)))

       (testing "captures the result of output"
         (swap! buffer conj ":foobar")
         (check (async/<! output) => ":foobar"))

       (testing "captures results of simple evaluations"
         (swap! control update :pending-evals conj 'id01)
         (swap! buffer conj "[tooling$eval-res id01 \":foo\"]")
         (check (async/<! results) => '[id01 ":foo"])

         (swap! buffer conj "[tooling$eval-res id01 \":foo\"]")
         (check (async/<! output) => "[tooling$eval-res id01 \":foo\"]"))

       (testing "captures results of results mixed with stdout"
         (swap! control update :pending-evals conj 'id01)
         (swap! buffer conj "lol[tooling$eval-res id01 \":foo\"]")
         (check (async/<! results) => '[id01 ":foo"])
         (check (async/<! output) => "lol")

         (swap! control update :pending-evals conj 'id01)
         (swap! buffer conj "lol[tooling$eval-res id01 \":foo\"]bar")
         (check (async/<! results) => '[id01 ":foo"])
         (check (async/<! output) => "lol")
         (check (async/<! output) => "bar"))

       (testing "ignores prompt after a result"
         (swap! control assoc :ignore-prompt true)
         (swap! control update :pending-evals conj 'id01)
         (swap! buffer conj "lol[tooling$eval-res id01 \":foo\"]\nuser.cljs=> ")
         (check (async/<! results) => '[id01 ":foo"])
         (check (async/<! output) => "lol")

         (swap! control update :pending-evals conj 'id01)
         (swap! buffer conj "[tooling$eval-res id01 \":foo\"]user.cljs=> bar")
         (check (async/<! results) => '[id01 ":foo"])
         (check (async/<! output) => "bar"))

       (testing "captures output in different fragments"
         (swap! control update :pending-evals conj 'id01)
         (swap! buffer conj "[tooling$eval-res id01 \"[\n")
         (swap! buffer conj "1 2\n")
         (swap! buffer conj "]\"]")
         (check (async/<! results) => '[id01 "[\n1 2\n]"]))

       (async/close! output)
       (async/close! results)
       (done)))))

(cards/deftest repl-evaluation
  (async done
    (async/go
     (some-> @state :conn .end)
     (reset! state (c/connect! "localhost" 2233))

     (check (async/<! (check-string #"shadow.user"))
            => #"shadow.user")
     (some-> @state :conn .end)
     (done))))

(cards/defcard-rg buffers
  (fn [buffer]
    [:div (pr-str @buffer)])
  (some-> @state :buffer)
  {:watch-atom true})
