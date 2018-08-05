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
  (prn [:NEXT-EVAL (:state @state)])
  (when (= (:state @state) :ready)
    (when-let [eval (-> @state :pending first)]
      (prn [:EVALUATING eval])
      (swap! state (fn [s]
                     (-> s
                         (update :pending #(->> % (drop 1) vec))
                         (assoc :processing eval)
                         (assoc :state :evaluating))))
      (async/put! (:channel-in @state) (:cmd eval)))))

(defn- add-to-eval-queue! [id chan cmd state]
  (prn [:ADD-TO-EVAL @state])
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

  (break [this id]))
    ; (let [interrupt (->> @session :pending-evals
    ;                       (filter #(= id (:id %)))
    ;                       first :interrupt)]
    ;   (when interrupt
    ;     ; RESET connection, because UNREPL's interrupt is not working!
    ;     ; (async/put! @in (str interrupt "\n"))
    ;     (doseq [pending (-> @session :pending-evals)]
    ;       (async/put! (:channel pending) {}))
    ;     (prn :disconnecting)
    ;     (client/disconnect! (:session-name @session))
    ;     (let [evaluator (repl (:session-name @session)
    ;                           (:host @session) (:port @session)
    ;                           (:on-output @session))]
    ;       (reset! in @(:in evaluator))
    ;       (swap! session (constantly @(:session evaluator)))
    ;       (prn :all-reset))))))

        ; (prn [:sending (str interrupt "\n")])
        ; (prn [:sending (pr-str interrupt)])
        ; (prn :DONE)))))

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
    (prn [:TREATING cmd])
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
    (add-watch session 1 (fn [_key _ref old-value new-value]
                          (prn [:OLD old-value])
                          (prn [:NEW new-value])))
    (async/put! in blob)
    (go-loop [string (<! out)]
      (treat-all-output! string state)
      (recur (<! out)))
    (->Evaluator session)))
