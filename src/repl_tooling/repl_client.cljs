(ns repl-tooling.repl-client
  (:require [cljs.core.async :refer [chan <! >!] :refer-macros [go-loop go]]
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

(defn p [a] (prn [:in a]) a)
(defn integrate-repl [in out repl]
  (let [n-in (chan)]
    (go-loop []
      (>! in (p (repl/cmd-to-send repl (str (<! n-in)))))
      (recur))
    [n-in out]))

; (defn connect-lumo! [session-name host port]
;   (let [[in out socket] (socket! session-name host port)
;         repl (lumo/->Lumo in out socket)]
;     (integrate-repl in repl socket)))
