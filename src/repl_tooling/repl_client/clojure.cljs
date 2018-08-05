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

(defn- next-eval! [state]
  (when (= (:state @state) :ready)
    (when-let [eval (-> @state :pending first)]
      (swap! state (fn [s]
                     (-> s
                         (update :pending #(->> % (drop 1) vec))
                         (assoc :processing eval)
                         (assoc :state :evaluating))))
      (async/put! (:channel-in @state) (str (:cmd eval) "\n")))))

(defn- add-to-eval-queue! [id chan cmd state]
  (swap! state update :pending conj {:cmd cmd :channel chan :id id})
  (next-eval! state))

(declare repl)
(defrecord Evaluator [session]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (let [id (gensym)
          chan (async/chan)
          {:keys [filename row col namespace]} opts]
      (add-to-eval-queue! id chan command (:state @session))
      (go (callback (<! chan)))
      id))

  (break [this id]
    (when (-> @session :state deref :processing :id (= id))
      ; RESET connection, because UNREPL's interrupt is not working!
      ; First, clear all pending evals
      (some-> @session :state deref :processing :channel (doto
                                                           (async/put! {})
                                                           (async/close!)))

      (doseq [pending (-> @session :state deref :pending-evals)]
        (async/put! (:channel pending) {})
        (async/close! (:channel pending)))

      (client/disconnect! (:session-name @session))
      (let [evaluator (repl (:session-name @session)
                            (:host @session) (:port @session)
                            (-> @session :state deref :on-output))]
        (reset! session @(:session evaluator))))))

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

(defn- eval-next! [state]
  (swap! state assoc :state :ready)
  (next-eval! state))

(defn- start-eval! [{:keys [actions]} state]
  (swap! state update :processing #(assoc %
                                            :interrupt (:interrupt actions)
                                            :background (:background actions))))

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

(defn- send-result! [res exception? state]
  (let [parsed (parse-res res)
        msg (assoc parsed (if exception? :error :result) (pr-str res))
        on-out (:on-output @state)]
    (on-out msg)
    (when-let [chan (-> @state :processing :channel)]
      (async/put! chan msg)
      (async/close! chan))))

(defn- send-output! [out state]
  (let [on-out (:on-output @state)]
    (on-out {:out out})))

(defn- treat-unrepl-message! [raw-out state]
  (let [[cmd args] (reader/read-string {:readers decoders :default default-tags} raw-out)]
    (case cmd
      :prompt (eval-next! state)
      :started-eval (start-eval! args state)
      :eval (send-result! args false state)
      :exception (send-result! args true state)
      :out (send-output! args state)
      :nothing-really)))

(defn- treat-hello! [hello state]
  (let [[_ res] (reader/read-string {:readers decoders} hello)]
    (swap! state assoc
           :session (:session res)
           :actions (:actions res))))

(defn- treat-all-output! [raw-out state]
  (if-let [hello (re-find #"\[:unrepl/hello.*" (str raw-out))]
    (treat-hello! hello state)
    (if (:session @state)
      (treat-unrepl-message! raw-out state))))
      ; ((:on-output @session) {:unexpected (str raw-out)}))))

(defn repl [session-name host port on-output]
  (let [[in out] (client/socket2! session-name host port)
        state (atom {:state :starting
                     :processing nil
                     :pending []
                     :channel-in in
                     :on-output on-output})
        session (atom {:state state
                       :session-name session-name
                       :host host
                       :port port})
        pending-cmds (atom {})]
    (async/put! in blob)
    (go-loop [string (<! out)]
      (treat-all-output! string state)
      (recur (<! out)))
    (->Evaluator session)))
