(ns repl-tooling.repl-client.clojure
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [blob-contents]])
  (:require [repl-tooling.repl-client.protocols :as repl]
            [repl-tooling.repl-client :as client]
            [cljs.core.async :as async :refer-macros [go go-loop]]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(def blob (blob-contents))

(defn- treat-all-output [raw-out treated-out session])

(defn connect-socket! [session-name host port]
  (let [[in out] (client/socket2! session-name host port)
        new-out (async/chan)
        session (atom {})
        param-decoder (fn [p] {:param p})
        pending-cmds (atom #{})]
    (async/put! in blob)
    (go-loop [output (<! out)]
      (.log js/console (str [:out output]))
      (if-let [hello (re-find #"\[:unrepl/hello.*" output)]
        (let [[_ res] (reader/read-string {:readers {'unrepl/params param-decoder}} hello)]
          (reset! session res))
        (recur 10)))
    [in out]))
;         repl (->Lumo pending-cmds)
;         [in out] (client/integrate-repl in out repl)
;         new-out (async/map #(treat-output pending-cmds %) [out])]
;     (async/put! in '(require 'lumo.repl))
;     (async/put! in "(set! lumo.repl/*pprint-results* false)")
;
;     [in new-out]))
