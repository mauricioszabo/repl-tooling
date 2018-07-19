(ns repl-tooling.repl-client.generic
  (:require [repl-tooling.repl-client.protocols :as repl]
            [repl-tooling.repl-client :as client]
            [cljs.core.async :refer [chan <! >!] :refer-macros [go-loop go]]
            [clojure.string :as str]))

(defn- parse-output2 [output]
  (let [parsed (some-> output not-empty pop)
        result (last parsed)
        out (cond-> parsed (-> parsed last (= result)) pop)]
    {:result result
     :out (str/join "\n" out)}))

(def ^:private buffer-txt (atom []))
(defrecord Generic [in out socket]
  repl/Repl
  (treat-data [_ data]
    (let [string (str data)]
      (swap! buffer-txt #(vec (concat (some-> % not-empty pop)
                                      (str/split (str (last %) string) #"\n"))))
      (when (str/ends-with? string "=> ")
        (let [output (parse-output2 @buffer-txt)]
          (go (>! out output))
          (reset! buffer-txt [])))))

  (send-command [_ command]
    (.write socket command)))

(defn connect-socket! [session-name host port]
  (let [[in out socket] (client/socket! session-name host port)
        repl (->Generic in out socket)]
    (client/integrate-repl in repl socket)
    [in out]))
