(ns repl-tooling.repl-client
  (:require [cljs.core.async :refer [chan <! >!] :refer-macros [go-loop go]]
            [clojure.string :as str]))

(def ^:private net (js/require "net"))

(def ^:private sessions (atom {}))

(defn disconnect! [session-name]
  (when-let [socket (get @sessions session-name)]
    (.end socket)
    (swap! sessions dissoc session-name)))

(defn- parse-output [output]
  (let [result (some-> output not-empty pop)]
    {:out (str/join "\n" result)}))

(def ^:private buffer-txt (atom []))
(defn- accumulate [out data]
  (let [string (str data)]
    (swap! buffer-txt #(vec (concat (some-> % not-empty pop)
                                    (str/split (str (last %) string) #"\n"))))
    (when (str/ends-with? string "=> ")
      (let [output (parse-output @buffer-txt)]
        (go (>! out output))
        (reset! buffer-txt [])))))

(defn connect-socket! [session-name host port]
  (let [in (chan)
        out (chan)
        socket (doto (. net createConnection port host)
                     (.on "data" #(accumulate out %)))]

    (swap! sessions assoc session-name socket)
    (go-loop []
      (let [data (str (<! in))
            to-send (cond-> data (not (str/ends-with? data "\n")) (str "\n"))]
        (.write socket to-send))
      (recur))
    [in out]))
