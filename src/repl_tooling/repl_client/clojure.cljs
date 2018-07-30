(ns repl-tooling.repl-client.clojure
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [blob-contents]])
  (:require [repl-tooling.repl-client.protocols :as repl]
            [repl-tooling.repl-client :as client]
            [repl-tooling.eval :as eval]
            [cljs.core.async :as async :refer-macros [go go-loop]]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(def blob (blob-contents))

(defrecord Evaluator [in out session]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (let [id (gensym)
          chan (async/chan)
          {:keys [filename row col namespace]} opts]
      (swap! session #(update % :pending-evals conj {:channel chan :id id}))
      (go (callback (<! chan)))
      (async/put! in command)
      id))

  (break [this id]
    (let [interrupt (->> @session :pending-evals
                          (filter #(= id (:id %)))
                          first :interrupt)]
      (when interrupt
        (eval/evaluate this interrupt {} identity)))))

(def ^:private decoders
  (let [param-decoder (fn [p] {:param p})
        ns-decoder identity]
    {'unrepl/param param-decoder
     'unrepl/ns ns-decoder}))

(defn- treat-hello! [hello session]
  (let [param-decoder (fn [p] {:param p})
        [_ res] (reader/read-string {:readers decoders} hello)]
    (-> session
        (swap! assoc
               :session (:session res)
               :actions (:actions res)))))

(defn- send-result! [parsed session]
  (let [chan (-> @session :pending-evals first :channel)
        res (-> parsed second prn-str str/trim)]
    (swap! session update :pending-evals #(vec (drop 1 %)))
    ((:on-output @session) {:result res})
    (async/put! chan res)
    (async/close! chan)))

(defn- treat-unrepl-message! [raw-out session]
  (let [parsed (reader/read-string {:readers decoders} raw-out)]
    (case (first parsed)
      :started-eval (swap! session update-in [:pending-evals 0] assoc
                           :interrupt (-> parsed second :actions :interrupt))
      :eval (send-result! parsed session)
      :out ((:on-output @session) {:out (second parsed)})
      :nothing-really)))

(defn- treat-all-output! [raw-out session]
  (if-let [hello (re-find #"\[:unrepl/hello.*" (str raw-out))]
    (treat-hello! hello session)
    (if (:session @session)
      (treat-unrepl-message! raw-out session)
      (prn [:out raw-out]))))

; (defn connect-socket! [session-name host port]
;   (let [[in out] (client/socket2! session-name host port)
;         new-out (async/chan)
;         session (atom {:pending-evals []})
;         pending-cmds (atom {})]
;     (async/put! in blob)
;     (go-loop [output (<! out)]
;       (treat-all-output! output session)
;       (recur (<! out)))
;     [in new-out]))

(defn repl [session-name host port on-output]
  (let [[in out] (client/socket2! session-name host port)
        session (atom {:pending-evals []
                       :on-output on-output})
        pending-cmds (atom {})]
    (async/put! in blob)
    (go-loop [output (<! out)]
      ; (prn [:loop-out output])
      (treat-all-output! output session)
      (recur (<! out)))
    (->Evaluator in out session)))
