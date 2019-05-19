(ns repl-tooling.repl-client.clojure
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [blob-contents]])
  (:require [repl-tooling.repl-client.protocols :as repl]
            [repl-tooling.repl-client :as client]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [cljs.core.async :as async :refer-macros [go go-loop]]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def blob (blob-contents))

; Pending eval format:
; {:cmd string? :channel async :id symbol? :ignore-result? boolean :opts map?}
(defn- next-eval! [state]
  (when (and (= (:state @state) :ready)
             (-> @state :processing nil?))
    (when-let [cmd (-> @state :pending first)]
      (swap! state (fn [s]
                     (-> s
                         (update :pending #(-> % rest vec))
                         (assoc :processing cmd :state :evaluating))))
      (async/put! (:channel-in @state) (:cmd cmd)))))

(defn- add-to-eval-queue! [id chan cmd state ignore? opts]
  (swap! state update :pending conj {:cmd cmd
                                     :channel chan
                                     :id id
                                     :ignore-result? ignore?
                                     :opts opts})
  (next-eval! state))

(defn unrepl-cmd [state command params]
  (let [mapping (->> params
                     (map (fn [[k v]] [{:repl-tooling/param k} v]))
                     (into {}))]
    (->> @state :actions command (walk/postwalk-replace mapping))))

(defn- prepare-opts [repl {:keys [filename row col namespace]}]
  (let [state (-> repl :session deref :state)
        params {:unrepl/sourcename (str filename)
                :unrepl/column (-> col (or 1) dec)
                :unrepl/line (-> row (or 1) dec)}]
    (when namespace
      (add-to-eval-queue! (gensym) (async/promise-chan) (str "(ns " namespace ")") state true {}))
    (when (or filename row col)
      (add-to-eval-queue! (gensym) (async/promise-chan)
                          (unrepl-cmd state :set-source params)
                          state
                          true
                          {}))))

(declare repl)
(defrecord Evaluator [session]
  eval/Evaluator
  (evaluate [this command opts callback]
    (let [id (gensym)
          chan (async/promise-chan)
          state (:state @session)]
      (prepare-opts this opts)
      (add-to-eval-queue! id chan (str "(do\n" command "\n)") state (:ignore opts) (:pass opts))
      (go (callback (<! chan)))
      id))

  (break [this id]))

(defn- default-tags [tag data]
  (helpers/WithTag. data tag))

(deftype IncompleteStr [string]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer (pr-str (str (first string) " ..."))))

  IMeta
  (-meta [coll] {:get-more (-> string second :repl-tooling/...)}))

(def ^:private decoders
  (let [param-decoder (fn [p] {:repl-tooling/param p})
        more-decoder (fn [{:keys [get]}] {:repl-tooling/... get})
        ns-decoder identity]
    {'unrepl/param param-decoder
     'class identity
     'unrepl/... more-decoder}))

(defn- eval-next! [state]
  (swap! state assoc :state :ready)
  (next-eval! state))

(defn- start-eval! [{:keys [actions]} state]
  (swap! state update :processing #(assoc %
                                          :interrupt (:interrupt actions)
                                          :background (:background actions))))

(defn- parse-res [result]
  (let [to-string #(cond
                     (and (instance? helpers/WithTag %) (-> % helpers/tag (= "#unrepl/string ")))
                     (-> % helpers/obj first (str "..."))

                     (and (map? %) (:repl-tooling/... %))
                     (with-meta '... {:get-more (:repl-tooling/... %)})

                     :else
                     %)]
    (if (coll? result)
      {:as-text (pr-str (walk/prewalk to-string result))}
      {:as-text (pr-str (to-string result))})))

(defn- send-result! [res exception? state]
  (let [parsed (parse-res res)
        msg (->> (pr-str res)
                 (assoc parsed (if exception? :error :result))
                 (merge (-> @state :processing :opts)))
        on-out (:on-output @state)]
    (when-not (-> @state :processing :ignore-result?)
      (on-out msg))
    (when-let [chan (-> @state :processing :channel)]
      (swap! state assoc :processing nil)
      (async/put! chan msg))))

(defn- send-output! [out state err?]
  (let [on-out (:on-output @state)]
    (if err?
      (on-out {:err out})
      (on-out {:out out}))))

(defn- treat-unrepl-message! [raw-out state]
  (let [parsed (try (reader/read-string {:readers decoders :default default-tags} raw-out)
                 (catch :default e))
        [cmd args] (when (vector? parsed) parsed)]
    (case cmd
      :prompt (eval-next! state)
      :started-eval (start-eval! args state)
      :eval (send-result! args false state)
      :exception (send-result! args true state)
      :out (send-output! args state false)
      :err (send-output! args state true)
      :nothing-really)))

(defn- treat-hello! [hello state]
  (let [[_ res] (reader/read-string {:readers decoders} hello)]
    (swap! state assoc
           :session (:session res)
           :actions (:actions res))))

(defn- treat-all-output! [raw-out state]
  ; (prn [:RAW (str raw-out)])

  (if-let [hello (re-find #"\[:unrepl/hello.*" (str raw-out))]
    (treat-hello! hello state)
    (if (:session @state)
      (treat-unrepl-message! raw-out state)
      (some-> @state :session deref :on-output (#(% {:unexpected (str raw-out)}))))))

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
      (if string
        (do
          (treat-all-output! string state)
          (recur (<! out)))
        (on-output nil)))
    (->Evaluator session)))

(defrecord SelfHostedCljs [evaluator pending]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (let [id (gensym)
          in (-> evaluator :session deref :state deref :channel-in)
          code (str "(cljs.core/pr-str (try (clojure.core/let [res (do\n" command
                    "\n)] ['" id " :result (cljs.core/pr-str res)]) (catch :default e "
                    "['" id " :error (cljs.core/pr-str e)])))\n")]

      (swap! pending assoc id {:callback callback :ignore (:ignore opts)
                               :pass (:pass opts)})

      (when-let [ns-name (:namespace opts)]
        (async/put! in (str "(in-ns '" ns-name ")")))

      (async/put! in code)
      (swap! (:session evaluator) assoc :pending [])
      id))

  (break [this id]))

(defn- treat-result-of-call [out pending output-fn buffer]
  (let [full-out (str @buffer out)
        [_ id] (re-find #"^\"\[(.+?) " full-out)]
    (if-let [pendency (some->> id symbol (get @pending))]
      (if (str/ends-with? full-out "\n")
        (let [[_ key parsed] (->> full-out
                                  reader/read-string
                                  (reader/read-string {:default default-tags}))]
          (reset! buffer nil)
          ((:callback pendency) (assoc (:pass pendency) key parsed))
          (swap! pending dissoc id)
          (when-not (:ignore pendency) (output-fn {:as-text out :result parsed})))
        (swap! buffer str out))
      (do
        (reset! buffer nil)
        (output-fn {:out full-out})))))

(defn- pending-evals-for-cljs [pending output-fn buffer]
  (fn [{:keys [out]}]
    (if (or @buffer (and out (str/starts-with? out "\"[")))
      (treat-result-of-call out pending output-fn buffer)
      (when-not (or (= out "nil\n") (re-matches #"\[\d+:1\]~.+=>\s*" (str out)))
        (output-fn {:out out})))))

(defn self-host [clj-evaluator command]
  (let [pending (atom {})
        buffer (atom nil)
        cljs-repl (->SelfHostedCljs clj-evaluator pending)
        old-fn (-> clj-evaluator :session deref :state deref :on-output)]

    (swap! (-> clj-evaluator :session deref :state)
           assoc :on-output (pending-evals-for-cljs pending old-fn buffer))
    (js/Promise. (fn [resolve]
                   (eval/evaluate clj-evaluator command {}
                                  (fn [res]
                                    (let [res (helpers/parse-result res)]
                                      (resolve {:error (-> res :error
                                                           (or (:result res)))}))))
                   ; CLJS self-hosted REPL never returns, so we'll just set a timeout
                   (js/setTimeout #(resolve cljs-repl) 500)))))
