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
          ; res {:conn nil :buffer buffer}
      (async/go
        (c/treat-buffer! buffer #(async/put! lines %) #(async/put! frags %))
        (swap! buffer conj "foo")
        (swap! buffer conj "bar")
        (swap! buffer conj "b\nbaz")
        (check (async/<! lines) => "foobarb")
        (check @buffer => ["baz"])

        (async/close! lines)
        (async/close! frags)
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
