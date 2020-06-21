(ns repl-tooling.repl-client.shadow-ws
  (:require [schema.core :as s]
            [clojure.string :as str]
            [promesa.core :as p]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.integrations.repls :as repls]
            [cognitect.transit :as t]
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

(defn- evaluate! [state namespace code opts]
  (let [ws (:ws @state)
        row (:row opts 0)
        file (:filename opts "[EVAL]")
        build-id (:build-id @state)
        client-id (-> @state (get-in [:build->id build-id]) first)
        prom (p/deferred)]
    (if client-id
      (do
        (swap! state update :pending-evals assoc (:id opts) {:promise prom
                                                             :file file
                                                             :row row
                                                             :pass (:pass opts)})
        (send! ws {:op :cljs-eval
                   :to client-id
                   :call-id (:id opts)
                   :input {:code code :ns (symbol namespace)}}))
      (p/resolve! prom (merge (:pass opts)
                              (helpers/error-result "No clients connected"
                                                    (str "No clients connected to "
                                                         "the runtime " build-id)
                                                    [[file "" build-id row]]))))
    prom))

(defrecord ShadowCLJS [state]
  eval/Evaluator
  (evaluate [this command opts callback]
    (p/let [id (:id opts (gensym "shadow-eval-"))
            namespace (str (:namespace opts "cljs.user"))
            prom (evaluate! state namespace (str command) (assoc opts :id id))]
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
      (send! ws {:op :runtime-print-unsub :to id}))
    (when-let [id (-> builds build-id first)]
      (send! ws {:op :runtime-print-sub :to id}))))

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

(defn- capture-result! [state {:keys [result call-id]}]
  (when-let [{:keys [promise success? pass]} (-> @state :pending-evals (get call-id))]
    (swap! state update :pending-evals dissoc call-id)
    (p/resolve! promise (assoc pass
                               (if success? :result :error) result
                               :as-text result))))

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

(defn- send-as-error! [state {:keys [warnings call-id]}]
  (when-let [{:keys [promise row file]} (-> @state :pending-evals (get call-id))]
    (let [trace (->> warnings
                     (mapv (fn [{:keys [msg line]}]
                             [(str/replace msg #"Use of.* (.*/.*)$" "$1")
                              ""
                              file
                              (dec (+ row line))])))]
      (swap! state update :pending-evals dissoc call-id)
      (p/resolve! promise (helpers/error-result "Compile Warning"
                                                (->> warnings (map :msg) (str/join "\n"))
                                                trace)))))

(defn- obj-not-found! [state {:keys [call-id]}]
  (when-let [{:keys [promise row file]} (-> @state :pending-evals (get call-id))]
    (swap! state update :pending-evals dissoc call-id)
    (p/resolve! promise (helpers/error-result "404"
                                              "Result not found"
                                              [[file "" "" row]]))))

(defn- send-output! [state {:keys [stream text]}]
  (let [on-out (:on-output @state)
        key (if (= :stdout stream) :out :err)]
    (on-out {key text})))

(defn- compile-error! [state msg]
  (let [build-id (:build-id @state)
        on-out (:on-output @state)
        build-status (:build-status msg)]
    (when (= build-id (:build-id msg))
      (if (-> build-status :status (= :failed))
        (on-out {:err (:report build-status)})
        (when-let [warnings (not-empty (:warnings build-status))]
          (on-out {:compile-err {:type :warnings :warnings warnings}}))))))

(s/defn ^:private treat-ws-message! [state :- State, {:keys [op] :as msg}]
  (case op
    :welcome (send-hello! state)
    :clients (parse-clients! state msg)
    :notify (update-builds! state msg)
    :ping (send! (:ws @state) {:op :pong})
    :eval-result-ref (get-result! state msg)
    :eval-runtime-error (get-error! state msg)
    :obj-result (capture-result! state msg)
    :eval-compile-warnings (send-as-error! state msg)
    :eval-compile-error (get-error! state (assoc msg :from (:ex-client-id msg 1)))
    :obj-not-found (obj-not-found! state msg)
    :runtime-print (send-output! state msg)
    :shadow.cljs.model/sub-msg (compile-error! state msg)
    (prn :unknown-op op)))

(defn- create-ws-conn! [id url state]
  (try
    (let [ws (Websocket. url)
          update-state! (fn []
                          (swap! state assoc :ws ws)
                          (swap! repls/connections assoc id {:conn ws :buffer (atom [])}))]
      (update-state!)
      (doto ws
            (aset "onmessage" #(let [reader (t/reader :json)
                                     payload (->> ^js % .-data (t/read reader))]
                                 (treat-ws-message! state payload)))
            (aset "onclose" (fn [_]
                              (let [{:keys [on-output should-disconnect?]} @state]
                                (if should-disconnect?
                                  (on-output nil)
                                  (when-not (create-ws-conn! id url state)
                                    (on-output nil))))))
            (aset "end" (fn [_]
                          (swap! state assoc :should-disconnect? true)
                          (.close ws)))
        ws))
    (catch :default _
      nil)))

(defn connect! [{:keys [id build-id host port token on-output]}]
  (let [p (p/deferred)
        state (atom {:build-id build-id :should-disconnect? false
                     :evaluator p
                     :on-output (or on-output identity) :pending-evals {}
                     :build->id {} :id->build {}})
        ws (create-ws-conn! id
                            (str "ws://" host ":" port
                                 "/api/remote-relay?server-token=" token)
                            state)]
    (if ws
      p
      (p/promise nil))))
