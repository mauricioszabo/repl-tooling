(ns repl-tooling.repl-client
  (:require [cljs.core.async :refer [<! >!] :refer-macros [go-loop go] :as async]
            [repl-tooling.repl-client.protocols :as repl]
            [clojure.string :as str]))

(def ^:private net (js/require "net"))
(def ^:private sessions (atom {}))

(defn disconnect! [session-name]
  (when-let [socket (get @sessions session-name)]
    (.end socket)
    (swap! sessions dissoc session-name)))

(defn socket! [session-name host port]
  (let [[in out socket] (repl/connect-socket! host port)]

    (swap! sessions assoc session-name socket)
    [in out]))

(defn integrate-repl [in out repl]
  (let [n-in (async/chan)]
    (go-loop []
      (>! in (repl/cmd-to-send repl (str (<! n-in))))
      (recur))
    [n-in out]))
