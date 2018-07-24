(ns repl-tooling.repl-client.lumo
  (:require [repl-tooling.repl-client.protocols :as repl]
            [repl-tooling.repl-client :as client]
            [cljs.core.async :as async :refer-macros [go]]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(defn- code-to-lumo [identifier code]
  (let [reader (str (gensym) "reader" (gensym))
        result (str (gensym) "result" (gensym))]
    (str "(let [" reader " (goog.string/StringBuffer.)]
            (binding [cljs.core/*print-newline* true
                      lumo.repl/*pprint-results* false
                      cljs.core/*print-fn* (fn [x] (.append " reader " x))]
        (let [" result " (cljs.core/eval '" code "\n)]
          ['" identifier "
           (cljs.core/str " reader ")
           (cljs.core/str " result ")])))")))

(defrecord Lumo [pending-cmds]
  repl/Repl
  (cmd-to-send [_ command]
    (let [[id cmd] (if (str/starts-with? command "[")
                     (reader/read-string command)
                     [(gensym) command])
          command (code-to-lumo id cmd)]
      (swap! pending-cmds conj (str id))
      command)))

(defn- treat-output [pending-cmds out]
  (let [[_ match] (re-find #"^\s*\[(.+?) " out)]
    (if (@pending-cmds match)
      (let [[_ out result] (reader/read-string out)]
        (swap! pending-cmds disj match)
        {:id match :out out :result result})
      {:out out})))

(defn connect-socket! [session-name host port]
  (let [[in out] (client/socket! session-name host port)
        pending-cmds (atom #{})
        repl (->Lumo pending-cmds)
        [in out] (client/integrate-repl in out repl)
        new-out (async/map #(treat-output pending-cmds %) [out])]
    (async/put! in '(require 'lumo.repl))
    (async/put! in "(set! lumo.repl/*pprint-results* false)")

    [in new-out]))
