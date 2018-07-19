(ns repl-tooling.repl-client.protocols
  (:require [cljs.core.async :refer [chan <! >!] :refer-macros [go-loop go]]
            [clojure.string :as str]))

(defprotocol Repl
  (treat-data [_ data])
  (send-command [_ command]))

; (str/split "foo\nbar\nbaz\n" #"\n" 2)
; (str/split "a\n" #"\n" 2)

(defn- treat-result [buffer out fragment data]
  (let [string (str data)
        [first-line rest] (str/split string #"\n" 2)]
    (if rest
      (do
        (reset! buffer "")
        (go (>! out (str first-line "\n")))
        (recur buffer out rest))
      (do
        (swap! buffer str first-line)
        (go (>! fragment first-line))))))

(defn- write-into [socket data buffer out]
  (let [lines (-> data str str/trim (str/split #"\n"))]
    (reset! buffer "")
    (go
     (doseq [line lines]
       (prn [:in line])
       (.write socket (str line "\n"))
       (prn [:out (<! out)])))))
      ; (reset! buffer ""))))

(def ^:private net (js/require "net"))
(defn connect-socket! [host port]
  (let [in (chan)
        fragment (chan)
        out (chan)
        buffer (atom "")
        socket (doto (. net createConnection port host)
                     (.on "data" #(treat-result buffer out fragment %)))]
    (go-loop []
      (write-into socket (<! in) buffer fragment)
      (recur))
    [in out socket]))
