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

(defn- default-tags [tag data]
  (with-meta data {:tag (str "#" tag)}))

(deftype IncompleteStr [str]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer (first str))
    (-write writer " ..."))

  IMeta
  (-meta [coll] {:get-more (-> str second :repl-tooling/...)}))

(def ^:private decoders
  (let [param-decoder (fn [p] {:param p})
        more-decoder (fn [{:keys [get]}] {:repl-tooling/... get})
        ns-decoder identity]
    {'unrepl/param param-decoder
     'unrepl/ns ns-decoder
     'unrepl.java/class identity
     'unrepl/... more-decoder
     'unrepl/string #(IncompleteStr. %)}))

(defn- treat-hello! [hello session]
  (let [param-decoder (fn [p] {:param p})
        [_ res] (reader/read-string {:readers decoders} hello)]
    (-> session
        (swap! assoc
               :session (:session res)
               :actions (:actions res)))))

(defn- send-result! [parsed session error? additional-info]
  (let [chan (-> @session :pending-evals first :channel)
        res (pr-str parsed)
        key (if error? :error :result)]
    (swap! session update :pending-evals #(vec (drop 1 %)))
    ((:on-output @session) {key res})
    (async/put! chan (assoc additional-info key res))
    (async/close! chan)))

(defn- parse-res [result]
  (let [to-string #(cond
                     (instance? IncompleteStr %)
                     %

                     (not (coll? %)) (pr-str %)

                     (and (map? %) (:repl-tooling/... %))
                     (with-meta '... {:get-more (:repl-tooling/... %)})

                     :else %)]
    (if (coll? result)
      {:as-text (walk/prewalk to-string result)}
      {:as-text (to-string result)})))

(defn- treat-unrepl-message! [raw-out session]
  (let [parsed (reader/read-string {:readers decoders :default default-tags} raw-out)]
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

(defn repl [session-name host port on-output]
  (let [[in out] (client/socket2! session-name host port)
        session (atom {:pending-evals []
                       :on-output on-output})
        pending-cmds (atom {})]
    (async/put! in blob)
    (go-loop [string (<! out)]
      (treat-all-output! string session)
      (recur (<! out)))
    (->Evaluator in out session)))
