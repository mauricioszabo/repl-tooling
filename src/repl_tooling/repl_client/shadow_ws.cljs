(ns repl-tooling.repl-client.shadow-ws
  (:require [schema.core :as s]
            [clojure.string :as str]
            [promesa.core :as p]
            [repl-tooling.editor-helpers :as helpers]
            [clojure.reader :as edn]
            [repl-tooling.eval :as eval]
            [repl-tooling.integrations.repls :as repls]
            [cognitect.transit :as t]
            [repl-tooling.repl-client.clj-helper :as h]
            [repl-tooling.repl-client.source :as source]
            ["ws" :as Websocket]))

(def State (s/atom {:build-id s/Keyword
                    :on-output (s/=> s/Any s/Any)
                    :ws Websocket
                    :should-disconnect? s/Bool
                    :evaluator js/Promise
                    :id->build {s/Int s/Keyword}
                    :build->id {s/Keyword [s/Int]}
                    :pending-evals {s/Any {:promise s/Any
                                           :file s/Str
                                           :row s/Int
                                           :pass s/Any
                                           (s/optional-key :success?) s/Bool}}}))

(defn- send! [^js ws msg]
  (let [writer (t/writer :json)
        out (t/write writer msg)]
    (.send ws out)))

(def ^:private blob (h/contents-for-fn "cljs-cmd-wrap.cljs"))
(defn- evaluate! [state namespace code opts]
  (let [ws (:ws @state)
        row (:row opts 0)
        file (:filename opts "[EVAL]")
        build-id (:build-id @state)
        client-id (-> @state (get-in [:build->id build-id]) first)
        blobbed-code (if (:no-wrap opts)
                       code
                       (source/parse-command (str/replace blob #"__COMMAND__" code) true))
        prom (p/deferred)]
    (cond
      (:error blobbed-code)
      (p/resolve! prom (merge (:pass opts)
                              (helpers/error-result "Syntax Error"
                                                    (:error blobbed-code)
                                                    [[file nil build-id row]])))

      client-id
      (do
        (swap! state update :pending-evals assoc (:id opts) {:promise prom
                                                             :file file
                                                             :row row
                                                             :pass (:pass opts)})
        (send! ws {:op :cljs-eval
                   :to client-id
                   :call-id (:id opts)
                   :input {:code (:result blobbed-code) :ns (symbol namespace)}}))

      :else
      (p/resolve! prom (merge (:pass opts)
                              (helpers/error-result "No clients connected"
                                                    (str "No clients connected to "
                                                         "the runtime " build-id)
                                                    [[file nil build-id row]]))))
    prom))

(defn- send-custom-command! [state message id opts]
  (let [prom (p/deferred)
        row (:row opts 0)
        file (:filename opts "[EVAL]")
        message (assoc (edn/read-string message) :call-id id)]
    (swap! state update :pending-evals assoc (:call-id message) {:promise prom
                                                                 :file file
                                                                 :row row
                                                                 :pass (:pass opts)})
    (send! (:ws @state) message)
    prom))

(defrecord ShadowCLJS [state]
  eval/Evaluator
  (evaluate [this command opts callback]
    (p/let [id (:id opts (gensym "shadow-eval-"))
            namespace (-> opts :namespace str not-empty (or "cljs.user"))
            prom (if (:shadow-command opts)
                   (send-custom-command! state command id opts)
                   (evaluate! state namespace (str command) (assoc opts :id id)))]
      (callback prom)
      id))

  (break [this repl]))

(defn- send-hello! [state]
  (let [{:keys [ws evaluator]} @state]
    (send! ws {:op :hello :client-info {:editor :repl-tooling}})
    (send! ws {:op :request-clients
               :notify true
               :query [:and
                       [:eq :lang :cljs]
                       [:eq :type :runtime]]})
    (send! ws
           {:op :shadow.cljs.model/subscribe,
            :to 1,
            :shadow.cljs.model/topic :shadow.cljs.model/build-status-update})
    (p/resolve! evaluator (->ShadowCLJS state))))

(defn- listen-to-events! [state]
  (let [{:keys [ws build-id]} @state
        builds (:build->id @state)]
    (doseq [[_ ids] builds
            id ids]
      (send! ws {:op :runtime-print-unsub :to id})
      (send! ws {:op :tap-unsubscribe :to id}))
    (when-let [id (-> builds build-id first)]
      (send! ws {:op :runtime-print-sub :to id})
      (send! ws {:op :tap-subscribe :to id})
      (send! ws {:op :cljs-eval
                 :to id
                 :input {:code "(require 'cljs.reader)" :ns 'shadow.user}}))))

(defn- parse-clients! [state {:keys [clients]}]
  (let [build-id (:build-id @state)
        shadow-ids (->> clients
                        (group-by #(-> % :client-info :build-id))
                        (map (fn [v] (update v 1 #(mapv :client-id %))))
                        (into {}))]
    (swap! state assoc
           :build->id shadow-ids
           :id->build (into {} (for [[build-id ids] shadow-ids
                                     id ids]
                                 [id build-id])))
    (listen-to-events! state)))

(defn- add-id [st client-id build-id]
  (-> st
      (update-in [:build->id build-id] #(conj (or % []) client-id))
      (update :id->build assoc client-id build-id)))

(defn- remove-id [st client-id]
  (let [build-id (get-in st [:id->build client-id])]
    (-> st
        (update :id->build dissoc client-id)
        (update-in [:build->id build-id] #(->> %
                                               (remove (partial = client-id))
                                               vec)))))

(defn- update-builds! [state {:keys [event-op client-id client-info]}]
  (swap! state #(if (= :client-connect event-op)
                  (add-id % client-id (:build-id client-info))
                  (remove-id % client-id)))
  (listen-to-events! state))

(defn- resolve-pending! [state {:keys [call-id]} result]
  (let [{:keys [promise]} (-> @state :pending-evals (get call-id))]
    (swap! state update :pending-evals dissoc call-id)
    (p/resolve! promise result)))

(defn- capture-result! [state {:keys [result call-id] :as msg}]
  (when-let [{:keys [success? pass]} (-> @state :pending-evals (get call-id))]
    (let [parsed-res (if (str/starts-with? result "[tooling$eval-res")
                       (last (edn/read-string {:default tagged-literal} result))
                       {(if success? :result :error) result
                        :as-text result})]
      (resolve-pending! state msg (merge pass parsed-res)))))

(defn- get-result! [state msg]
  (swap! state update-in [:pending-evals (:call-id msg)] assoc :success? true)
  (send! (:ws @state) {:op :obj-request
                       :call-id (:call-id msg)
                       :to (:from msg)
                       :request-op :edn
                       :oid (:ref-oid msg)}))

(defn- get-error! [state msg]
  (swap! state update-in [:pending-evals (:call-id msg)] assoc :success? false)
  (send! (:ws @state) {:op :obj-request
                       :call-id (:call-id msg)
                       :to (:from msg)
                       :request-op :edn
                       :oid (:ex-oid msg)}))

(defn- send-as-error! [state {:keys [warnings call-id] :as msg}]
  (when-let [{:keys [row file]} (-> @state :pending-evals (get call-id))]
    (let [trace (->> warnings
                     (mapv (fn [{:keys [msg line]}]
                             [(str/replace msg #"Use of.* (.*/.*)$" "$1")
                              nil
                              file
                              (dec (+ row line))])))]
      (resolve-pending! state msg
                        (helpers/error-result "Compile Warning"
                                              (->> warnings (map :msg) (str/join "\n"))
                                              trace)))))

(defn- obj-not-found! [state {:keys [call-id] :as msg}]
  (when-let [{:keys [row file]} (-> @state :pending-evals (get call-id))]
    (resolve-pending! state msg (helpers/error-result "404"
                                                      "Result not found"
                                                      [[file nil nil row]]))))

(defn- send-output! [state {:keys [stream text]}]
  (let [on-out (:on-output @state)
        key (if (= :stdout stream) :out :err)]
    (on-out {key text})))

(defn- parse-compile-error-report [report]
  (let [prepare-stack (fn [[[_ file row col] [error]]]
                        {:file file :line row :column col :msg error})]
    (->> report
         str/split-lines
         (drop 1)
         (map (fn [row] (or (re-find #"File: (.*):(\d+):(\d+)" row)
                            (re-find #"^([^\s-].*)" row))))
         (filter identity)
         (partition 2 2)
         (map prepare-stack))))

(defn- compile-error! [state msg]
  (let [build-id (:build-id @state)
        on-out (:on-output @state)
        build-status (:build-status msg)]
    (when (= build-id (:build-id msg))
      (if (-> build-status :status (= :failed))
        (on-out {:compile-err {:type :errors :warnings (-> msg
                                                           :build-status
                                                           :report
                                                           parse-compile-error-report)}})
        (when-let [warnings (not-empty (:warnings build-status))]
          (on-out {:compile-err {:type :warnings :warnings warnings}}))))))

(defn- access-denied! [state]
  (.end ^js (:ws @state))
  (p/resolve! (:evaluator @state) {:error :access-denied}))

(defn- send-result! [state {:keys [pass]} msg]
  (let [res (pr-str msg)
        result (assoc pass :result res :as-text res)]
    (resolve-pending! state msg result)))

(defn- tap! [state msg]
  (send! (:ws @state) {:op :obj-request
                       :call-id (gensym "tap-result")
                       :to (:from msg)
                       :request-op :edn
                       :oid (:oid msg)}))

(defn- unexpected-obj! [state {:keys [result call-id]}]
  (let [on-out (:on-output @state)
        tapped? (str/starts-with? (str call-id) "tap-result")
        patch? (str/starts-with? result "#repl-tooling/patch")]
    (if (and tapped? patch?)
      (let [[id res] (edn/read-string {:readers {'repl-tooling/patch identity}} result)]
        (on-out {:patch {:id id :result {:as-text res :result res}}}))
      (on-out {:result {:id call-id
                        :result {:as-text result :result result}
                        :editor-data {:filename "<console>.cljs"
                                      :range [[0 0] [0 0]]
                                      :contents ""},
                        :range [[0 0] [0 0]]}}))))

(s/defn ^:private treat-ws-message! [state :- State, {:keys [op call-id] :as msg}]
  (if-let [pending (-> @state :pending-evals (get call-id))]
    (case op
      :obj-result (capture-result! state msg)
      :eval-runtime-error (get-error! state msg)
      :eval-compile-error (get-error! state (assoc msg :from (:ex-client-id msg 1)))
      :eval-compile-warnings (send-as-error! state msg)
      :eval-result-ref (get-result! state msg)
      :obj-not-found (obj-not-found! state msg)
      (send-result! state pending msg))
    (case op
      :welcome (send-hello! state)
      :clients (parse-clients! state msg)
      :notify (update-builds! state msg)
      :ping (send! (:ws @state) {:op :pong})
      :runtime-print (send-output! state msg)
      :shadow.cljs.model/sub-msg (compile-error! state msg)
      :access-denied (access-denied! state)
      :tap (tap! state msg)
      :obj-result (unexpected-obj! state msg)
      (prn :unknown-op op))))

(defn- create-ws-conn! [id url state]
  (try
    (let [ws (Websocket. url #js {:rejectUnauthorized false})
          update-state! (fn []
                          (swap! state assoc :ws ws)
                          (swap! repls/connections assoc id {:conn ws :buffer (atom [])}))]
      (update-state!)
      (doto ws
            (aset "onmessage" #(let [reader (t/reader :json)
                                     payload (->> ^js % .-data (t/read reader))]
                                 (treat-ws-message! state payload)))
            (aset "onerror" (fn [e]
                              (.end ws)
                              (.log js/console e)
                              (p/resolve! (:evaluator @state) {:error (.-message e)})))
            (aset "onclose" (fn [_]
                              (let [{:keys [on-output should-disconnect?]} @state]
                                (if should-disconnect?
                                  (on-output nil)
                                  (when (:error (create-ws-conn! id url state))
                                    (on-output nil))))))
            (aset "end" (fn [_]
                          (swap! state assoc :should-disconnect? true)
                          (.close ws))))
      ws)
    (catch :default e
      {:error (.-message e)})))

(defn connect! [{:keys [id build-id host port token on-output ssl?]}]
  (let [p (p/deferred)
        state (atom {:build-id build-id :should-disconnect? false
                     :evaluator p
                     :on-output (or on-output identity) :pending-evals {}
                     :build->id {} :id->build {}})
        ws (create-ws-conn! id
                            (str (if ssl? "wss://" "ws://")
                                 host ":" port
                                 "/api/remote-relay?server-token=" token)
                            state)]
    (if (:error ws)
      (p/promise ws)
      p)))
