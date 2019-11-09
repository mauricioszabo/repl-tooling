(ns repl-tooling.repl-client
  (:require [cljs.core.async :refer [<! >!] :refer-macros [go-loop go] :as async]
            [repl-tooling.repl-client.protocols :as repl]
            [clojure.string :as str]))

(defonce ^:private sessions (atom {}))

(defn disconnect! [session-name]
  (when-let [socket (get @sessions session-name)]
    (.end socket)
    (swap! sessions dissoc session-name)))

(defn socket! [session-name host port]
  (let [[in out socket] (repl/connect-socket! host port)]

    (swap! sessions assoc session-name socket)
    [in out]))

; FIXME! Really!
(defn socket2! [session-name host port]
  (let [[in out socket] (repl/connect-socket2! host port)]

    (swap! sessions assoc session-name socket)
    [in out]))
