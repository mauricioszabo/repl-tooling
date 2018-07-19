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
  (let [in (chan)
        out (chan)
        socket (. net createConnection port host)]

    (swap! sessions assoc session-name socket)
    [in out socket]))

(defn integrate-repl [in repl socket]
  (. socket on "data" #(repl/treat-data repl %))
  (go-loop []
    (let [data (str (<! in))
          to-send (cond-> data (not (str/ends-with? data "\n")) (str "\n"))]
      (repl/send-command repl to-send))
    (recur)))

; (defn connect-lumo! [session-name host port]
;   (let [[in out socket] (socket! session-name host port)
;         repl (lumo/->Lumo in out socket)]
;     (integrate-repl in repl socket)))
