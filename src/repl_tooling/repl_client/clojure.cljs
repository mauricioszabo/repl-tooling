(ns repl-tooling.repl-client.clojure
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [blob-contents]])
  (:require [repl-tooling.repl-client.protocols :as repl]
            [repl-tooling.repl-client :as client]
            [repl-tooling.eval :as eval]
            [cljs.core.async :as async :refer-macros [go go-loop]]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def blob (blob-contents))

(defrecord Evaluator [in out session]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (let [id (gensym)
          chan (async/chan)
          {:keys [filename row col namespace]} opts]
      (swap! session #(update % :pending-evals conj {:channel chan :id id}))
      (go (callback (<! chan)))
      (async/put! in (str command "\n"))
      id))

  (break [this id]
    (let [interrupt (->> @session :pending-evals
                          (filter #(= id (:id %)))
                          first :interrupt)]
      (when interrupt
        (eval/evaluate this interrupt {} identity)))))

(def ^:private decoders
  (let [param-decoder (fn [p] {:param p})
        more-decoder (fn [{:keys [get]}] {:repl-tooling/... get})
        ns-decoder identity]
    {'unrepl/param param-decoder
     'unrepl/ns ns-decoder
     'unrepl.java/class identity
     'unrepl/... more-decoder
     'unrepl/string first
     'error identity}))

(defn- treat-hello! [hello session]
  (let [param-decoder (fn [p] {:param p})
        [_ res] (reader/read-string {:readers decoders} hello)]
    (-> session
        (swap! assoc
               :session (:session res)
               :actions (:actions res)))))

(defn- send-result! [parsed session error? additional-info]
  (let [chan (-> @session :pending-evals first :channel)
        res (-> parsed prn-str str/trim)
        key (if error? :error :result)]
    (swap! session update :pending-evals #(vec (drop 1 %)))
    ((:on-output @session) {key res})
    (async/put! chan (assoc additional-info key res))
    (async/close! chan)))

(defn- parse-res [result]
  (let [to-s #(-> % prn-str (str/replace #"\n$" ""))
        to-string #(cond
                     (not (coll? %)) (to-s %)

                     (and (map? %) (:repl-tooling/... %))
                     (with-meta '... {:get-more (:repl-tooling/... %)})

                     :else %)]
    (if (coll? result)
      {:as-text (walk/prewalk to-string result)}
      {:as-text (prn-str result)})))

(defn- treat-unrepl-message! [raw-out session]
  (prn [:RAW raw-out])
  (def out raw-out)
  (let [parsed (reader/read-string {:readers decoders} raw-out)]
    (case (first parsed)
      :started-eval (swap! session update-in [:pending-evals 0] assoc
                           :interrupt (-> parsed second :actions :interrupt))
      :eval (send-result! (second parsed) session false (parse-res (second parsed)))
      :exception (send-result! (-> parsed second :ex) session true {})
      :out ((:on-output @session) {:out (second parsed)})
      :nothing-really)))

(defn- treat-all-output! [raw-out session]
  (if-let [hello (re-find #"\[:unrepl/hello.*" (str raw-out))]
    (treat-hello! hello session)
    (if (:session @session)
      (treat-unrepl-message! raw-out session))))
      ; ((:on-output @session) {:unexpected (str raw-out)}))))

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
    (go-loop []
      ; (prn [:loop-out output])
      (treat-all-output! (<! out) session)
      (recur))
    (->Evaluator in out session)))
