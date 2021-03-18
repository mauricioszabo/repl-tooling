(ns repl-tooling.repl-client.clojure
  (:require [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.repl-client.clj-helper :as h]
            [repl-tooling.eval :as eval]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [repl-tooling.repl-client.source :as source]
            [rewrite-clj.parser :as parser]))

(def clj-blob (h/blob-contents))

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
      (.write ^js (:conn @state) (str (:cmd cmd) "\n")))))

(defn- add-to-eval-queue! [state opts]
  (swap! state update :pending conj opts)
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
                :unrepl/line (-> row (or 1) dec dec)}]
    (when namespace
      (add-to-eval-queue! state
                          {:cmd (str "(if (find-ns '" namespace ")"
                                     "  (in-ns '" namespace ")"
                                     "  (ns " namespace "))")
                           :ignore-result? true}))
    (when (or filename row col)
      (add-to-eval-queue! state
                          {:cmd (unrepl-cmd state :set-source params) :ignore-result? true}))))

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
        more-decoder (fn [{:keys [get]}] {:repl-tooling/... get})]
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

(defrecord Evaluator [session]
  eval/Evaluator
  (evaluate [this command opts callback]
    (let [id (or (:id opts) (gensym))
          state (:state @session)
          err (try (parser/parse-string-all (str command)) nil
                (catch :default e {:error (pr-str (.-message e))
                                   :as-text (.-message e)}))
          eval-opts {:id id
                     :cmd (str "(do\n" command "\n)")
                     :callback callback
                     :ignore-result? (:ignore opts)
                     :opts (:pass opts)}]
      (if err
        (do
          ((:on-output @state) err)
          (callback err))
        (do
          (prepare-opts this opts)
          (add-to-eval-queue! state eval-opts)))
      id))

  (break [this repl]
    (when-let [interrupt (-> @session :state deref :processing :interrupt)]
      (eval/evaluate repl interrupt {:ignore true} identity))))

(defn- send-result! [res exception? state]
  (let [as-text (pr-str res)
        key (if exception? :error :result)
        msg (merge (-> @state :processing :opts)
                   {:as-text as-text key as-text})
        on-out (:on-output @state)]
    (when-not (-> @state :processing :ignore-result?)
      (on-out msg))
    (when-let [callback (-> @state :processing :callback)]
      (callback msg))
    (swap! state assoc :processing nil)))

(defn- send-output! [out state err?]
  (let [on-out (:on-output @state)]
    (if err?
      (on-out {:err out})
      (on-out {:out out}))))

(defn- send-patch! [[_ id res] state]
  (let [as-text (pr-str res)
        on-out (:on-output @state)]
    (on-out {:patch {:id id :result {:as-text as-text :result as-text}}})))

(defn- treat-unrepl-message! [raw-out state]
  (let [parsed (try (reader/read-string {:readers decoders :default default-tags} raw-out)
                 (catch :default _))
        [cmd args] (when (vector? parsed) parsed)]
    (case cmd
      :prompt (eval-next! state)
      :started-eval (start-eval! args state)
      :eval (send-result! args false state)
      :exception (send-result! args true state)
      :patch (send-patch! parsed state)
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
      (treat-unrepl-message! (re-find #"\[.*" (str raw-out)) state)
      (some-> @state :session deref :on-output (#(% {:unexpected (str raw-out)}))))))

(defn prepare-unrepl-evaluator [conn control on-output]
  (let [state (atom {:state :starting
                     :processing nil
                     :pending []
                     :conn conn
                     :on-output on-output})
        session (atom {:state state})]
    (.write conn clj-blob)
    (swap! control assoc
           :on-line #(if %
                       (treat-all-output! % state)
                       (on-output nil))
           :on-fragment identity)
    (->Evaluator session)))

(defn- eval-code [{:keys [evaluator id callback ^js conn code]} opts]
  (swap! (:pending evaluator) assoc id {:callback callback
                                        :ignore (:ignore opts)
                                        :pass (:pass opts)})
  (when-let [ns-name (:namespace opts)] (.write conn (str "(in-ns '" ns-name ")\n")))
  (.write conn (str (:result code) "\n"))
  (swap! (-> evaluator :evaluator :session) assoc :pending []))

(def ^:private cljs-blob (h/contents-for-fn "cljs-cmd-wrap.cljs"))
(defrecord SelfHostedCljs [evaluator pending]
  eval/Evaluator
  (evaluate [self command opts callback]
    (let [id (or (:id opts) (gensym))
          state (-> evaluator :session deref :state deref)
          conn (:conn state)
          code (source/wrap-command cljs-blob id command :default false)]

      (if (:no-wrap opts)
        (do
          (.write conn (str command "\n"))
          (callback {:result "nil" :as-text "nil"}))
        (if (:error code)
          (let [output (:on-output state)]
            (output code)
            (callback code))
          (eval-code {:evaluator self :id id :callback callback :conn conn :code code}
                     opts)))
      id))

  (break [this repl]))

(defn- treat-result-of-call [out pending output-fn buffer]
  (when (= ::ignore-next @buffer) (reset! buffer nil))
  (let [full-out (str @buffer out)
        [_ id] (re-find #"^\[tooling\$eval-res (.+?) " full-out)]
    (if-let [pendency (some->> id symbol (get @pending))]
      (if (or (str/ends-with? full-out "\n")
              (str/ends-with? full-out "\r\n"))
        (let [[_ _ parsed] (->> full-out
                                (reader/read-string {:default default-tags}))]
          (reset! buffer ::ignore-next)
          ((:callback pendency) (merge (:pass pendency) parsed))
          (swap! pending dissoc id)
          (when-not (:ignore pendency) (output-fn (merge (:pass pendency)
                                                         {:as-text (pr-str parsed)}
                                                         parsed))))
        (swap! buffer str out))
      (do
        (reset! buffer nil)
        (output-fn {:out full-out})))))

(defn- pending-evals-for-cljs [pending output-fn buffer]
  (fn [{:keys [out]}]
    (cond
      (and (= @buffer ::ignore-next) (re-find #"=> \r?\n?$" (str out)))
      (reset! buffer nil)

      (or @buffer (and out (str/starts-with? out "[tooling$eval-res")))
      (treat-result-of-call out pending output-fn buffer)

      (or (= out "nil\n") (= out "nil\r\n"))
      (reset! buffer ::ignore-next)

      :else
      (output-fn {:out out}))))

(defn- make-requires! [cljs-repl resolve]
  (eval/eval cljs-repl "(require 'clojure.string)" {:no-wrap true})
  (eval/eval cljs-repl "(require 'cljs.reader)" {:no-wrap true})
  (resolve cljs-repl))

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
                                    (if (contains? res :error)
                                      (helpers/parse-result res)
                                      (make-requires! cljs-repl resolve))))
                   ; CLJS self-hosted REPL SHOULD never return, so just set a timeout
                   ; TODO: Sometimes it DOES return, I have no idea why...
                   (js/setTimeout #(make-requires! cljs-repl resolve) 500)))))

(defn disable-limits! [aux]
  (if (instance? Evaluator aux)
    (.then (eval/eval aux ":disable-limits")
           #(eval/eval aux
                       (unrepl-cmd (-> aux :session deref :state)
                                   :print-limits
                                   {:unrepl.print/string-length 9223372036854775807
                                    :unrepl.print/coll-length 9223372036854775807
                                    :unrepl.print/nesting-depth 9223372036854775807})))
    (.resolve js/Promise {:result nil :as-text "nil"})))
