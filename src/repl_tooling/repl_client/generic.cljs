(ns repl-tooling.repl-client.generic
  (:require [repl-tooling.repl-client.protocols :as repl]
            [repl-tooling.repl-client :as client]
            [cljs.core.async :as async :refer [<! >!] :refer-macros [go-loop go]]
            [clojure.string :as str]))

(defn- parse-output [output]
  (let [parsed (some-> output not-empty)
        result (-> parsed last str/trim)
        out (if (and parsed (-> parsed last str/trim (= result)))
              (pop parsed)
              parsed)]
    {:result result
     :out (str/join "\n" out)}))

(defrecord Generic []
  repl/Repl
  (cmd-to-send [_ command] command))

(defn- treat-data [out to-world]
  (go-loop [data (<! out)]
    (loop [data data
           buffer []]
      (if data
        (recur (async/poll! out) (conj buffer (str data)))
        (>! to-world (parse-output buffer))))
    (recur (<! out))))

(defn connect-socket! [session-name host port]
  (let [[in out] (client/socket! session-name host port)
        repl (->Generic)
        new-out (async/chan)
        [in out] (client/integrate-repl in out repl)]
    (treat-data out new-out)
    [in new-out]))
