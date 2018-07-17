(ns repl-tooling.repl-client
  (:require [cljs.core.async :refer [chan <! >!] :refer-macros [go-loop go]]
            [clojure.string :as str]))

(def ^:private net (js/require "net"))

(def ^:private sessions (atom {}))

(defn disconnect! [session-name]
  (when-let [socket (get @sessions session-name)]
    (.end socket)
    (swap! sessions dissoc session-name)))

(defn- parse-output [output])
  ; (if)
  ; ())

(defn- treat-first-texts! [socket out]
  (let [listen (fn listen [output]
                 (when (str/ends-with? (str output) "=>")
                   (.off socket "data" listen)
                   (.on socket "data" #(go (>! out {:result (str %)})))))]

    (.on socket "data" listen)))


(defn connect-socket! [session-name host port]
  (let [in (chan)
        out (chan)
        socket (doto (. net createConnection 5550 "localhost")
                     (.on "close" #(println "OK")))]

    (swap! sessions assoc session-name socket)
    (treat-first-texts! socket out)
    (go-loop []
      (let [data (str (<! in))
            to-send (cond-> data (not (str/ends-with? data "\n")) (str "\n"))]
        (println "SENDING " to-send)
        (.write socket to-send))
      (recur))
    [in out]))
