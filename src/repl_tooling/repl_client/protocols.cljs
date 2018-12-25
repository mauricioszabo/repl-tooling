(ns repl-tooling.repl-client.protocols
  (:require [cljs.core.async :as async :refer [<! >!] :refer-macros [go-loop go]]
            [clojure.string :as str]))

(defprotocol Repl
  (cmd-to-send [_ command]))

(defn- pause-buffer! [buffer] (swap! buffer assoc :paused true))
(defn- resume-buffer! [buffer] (swap! buffer assoc :paused false))
(defn- reset-contents! [buffer] (swap! buffer assoc :contents ""))

(defn- update-buffer-and-send [buffer promises string]
  (let [[first-line frags] (str/split string #"\n" 2)
        contents (str (:contents @buffer) first-line)]
    (if frags
      (do
        (reset-contents! buffer)
        ; THIS IS THE ERROR
        (async/put! (first @promises) contents)
        (swap! promises rest)
        (recur buffer promises frags))
      (swap! buffer #(update % :contents str first-line)))))

(defn- treat-result [buffer promises fragment data]
  (let [string (str data)]
    (if (:paused @buffer)
      (async/put! fragment string)
      (update-buffer-and-send buffer promises string))))

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
     (.write socket (last lines))
     (async/alts! [fragment (async/timeout 150)])
     (resume-buffer! buffer)
     (.write socket "\n")
     (async/close! sync))))

(def ^:private net (js/require "net"))
(defn connect-socket! [host port]
  (let [in (async/chan)
        fragment (async/chan)
        promises (atom (iterate #(async/promise-chan) (async/promise-chan)))
        out (async/chan)
        _ (go-loop [[chan & others] @promises]
            (if-let [val (async/<! chan)]
              (do
                (async/>! out val)
                (recur others))
              (async/close! out)))
        sync (async/promise-chan)
        buffer (atom {:paused false :contents ""})
        socket (doto (. net createConnection port host)
                     (.on "data" #(treat-result buffer promises fragment %))
                     (.on "close" #(async/close! (first @promises))))]
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
        promises (atom (iterate #(async/promise-chan) (async/promise-chan)))
        out (async/chan)
        _ (go-loop [[chan & others] @promises]
            (if-let [val (async/<! chan)]
              (do
                (async/>! out val)
                (recur others))
              (async/close! out)))
        buffer (atom {:paused false :contents ""})
        socket (doto (. net createConnection port host)
                     (.on "data" #(treat-result buffer promises fragment %))
                     (.on "close" #(async/close! (first @promises))))]
    (go-loop []
      (let [string (str (<! in))]
        (.write socket string))
      (recur))

    [in out socket]))
