(ns repl-tooling.repl-client.protocols
  (:require [cljs.core.async :as async :refer [<! >!] :refer-macros [go-loop go]]
            [clojure.string :as str]))

(defprotocol Repl
  (cmd-to-send [_ command]))

(defn- pause-buffer! [buffer] (swap! buffer assoc :paused true))
(defn- resume-buffer! [buffer] (swap! buffer assoc :paused false))
(defn- reset-contents! [buffer] (swap! buffer assoc :contents ""))

(defn- update-buffer-and-send [buffer out string]
  (let [[first-line rest] (str/split string #"\n" 2)
        contents (str (:contents @buffer) first-line)]
    (if rest
      (do
        (reset-contents! buffer)
        (go (>! out contents))
        (recur buffer out rest))
      (swap! buffer #(update % :contents str first-line)))))

(defn- treat-result [buffer out fragment data]
  (let [string (str data)]
    (if (:paused @buffer)
      (go (>! fragment string))
      (update-buffer-and-send buffer out string))))

(defn- write-into [socket data buffer fragment sync]
  (let [lines (-> data str str/trim (str/split #"\n"))
        to-send (butlast lines)]

    (pause-buffer! buffer)
    (go
     (async/alts! [fragment (async/timeout 50)])
     (doseq [line to-send]
       (.write socket (str line "\n"))
       (while (not (re-find #"#_=>" (str/join " " (async/alts! [fragment
                                                                (async/timeout 500)]))))))
     (reset-contents! buffer)
     (resume-buffer! buffer)
     (.write socket (str (last lines) "\n"))
     (async/close! sync))))

(def ^:private net (js/require "net"))
(defn connect-socket! [host port]
  (let [in (async/chan)
        fragment (async/chan)
        out (async/chan)
        sync (async/promise-chan)
        buffer (atom {:paused false :contents ""})
        socket (doto (. net createConnection port host)
                     (.on "data" #(treat-result buffer out fragment %)))]
    (async/close! sync)
    (go-loop [sync sync]
      (let [code (<! in)
            new-sync (async/promise-chan)]
        (<! sync)
        (write-into socket code buffer fragment new-sync)
        (recur new-sync)))
    [in out socket]))

; FIXME! REALLY!
(defn connect-socket2! [host port]
  (let [in (async/chan)
        fragment (async/chan)
        out (async/chan)
        buffer (atom {:paused false :contents ""})
        socket (doto (. net createConnection port host)
                     (.on "data" #(treat-result buffer out fragment %))
                     (.on "close" #(do (prn :CLOSE) (async/close! out))))]
    (go-loop []
      (let [string (str (<! in))]
        (.write socket string))
      (recur))
    [in out socket]))
