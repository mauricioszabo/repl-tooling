(ns repl-tooling.repl-client.shadow-ws
  (:require [schema.core :as s]
            [promesa.core :as p]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.integrations.repls :as repls]
            [cognitect.transit :as t]
            ["ws" :as Websocket]))

(def State (s/atom {:build-id s/Keyword
                    :evaluator js/Promise
                    :id->build {s/Int s/Keyword}
                    :build->id {s/Keyword [s/Int]}
                    :pending-evals [{:promise s/Any
                                     (s/optional-key :success?) s/Bool}]}))

(defn- send! [^js ws msg]
  (let [writer (t/writer :json)
        out (t/write writer msg)]
    (.send ws out)))

(defn- evaluate! [ws state namespace code]
  (let [client-id (-> @state (get-in [:build->id (:build-id @state)]) first)
        prom (p/deferred)]
    (swap! state update :pending-evals conj {:promise prom})
    (prn :SENDING {:op :cljs-eval
                   :to client-id
                   :input {:code code :ns (symbol namespace)}})
    (send! ws {:op :cljs-eval
               :to client-id
               :input {:code code :ns (symbol namespace)}})
    prom))

(defrecord ShadowCLJS [ws state]
  eval/Evaluator
  (evaluate [this command opts callback]
    (p/let [id (gensym "shadow-eval-")
            namespace (str (:namespace opts "cljs.user"))
            prom (evaluate! ws state namespace (str command))]
      (callback prom)
      id))

  (break [this repl]))

(defn- send-hello! [ws state]
  (send! ws {:op :hello :client-info {:editor :repl-tooling}})
  (send! ws {:op :request-clients
             :notify true
             :query [:and
                     [:eq :lang :cljs]
                     [:eq :type :runtime]]})
  (p/resolve! (:evaluator @state) (->ShadowCLJS ws state)))

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
                                 [id build-id])))))

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
                  (remove-id % client-id))))

(defn- capture-result! [state {:keys [result]}]
  (when-let [{:keys [promise success?]} (-> @state :pending-evals first)]
    (swap! state update :pending-evals subvec 1)
    (p/resolve! promise {(if success? :result :error) result :as-text result})))

(defn get-result! [state ws msg]
  (swap! state update-in [:pending-evals 0] assoc :success? true)
  (send! ws {:op :obj-request
             :to (:from msg)
             :request-op :edn
             :oid (:ref-oid msg)}))

(defn get-error! [state ws msg]
  (swap! state update-in [:pending-evals 0] assoc :success? false)
  (send! ws {:op :obj-request
             :to (:from msg)
             :request-op :edn
             :oid (:ex-oid msg)}))

(s/defn ^:private treat-ws-message! [state :- State,
                                     ws :- Websocket,
                                     {:keys [op] :as msg}]
  (prn :MSG msg)
  (case op
    :welcome (send-hello! ws state)
    :clients (parse-clients! state msg)
    :notify (update-builds! state msg)
    :ping (send! ws {:op :pong})
    :eval-result-ref (get-result! state ws msg)
    :eval-runtime-error (get-error! state ws msg)
    :obj-result (capture-result! state msg)
    (prn :UNKNWOWN op)))

(defn connect! [id build-id host port token]
  (prn :CONNECTING id)
  ; (repls/disconnect! id)
  (let [ws (Websocket. (str "ws://" host ":" port
                            "/api/remote-relay?server-token=" token))
        p (p/deferred)
        state (atom {:build-id build-id :evaluator p
                     :build->id {} :id->build {} :pending-evals []})]
    (def state state)
    (def ws ws)
    (aset ws "end" (.-close ws))
    (swap! repls/connections assoc id {:conn ws :buffer (atom [])})
    (aset ws "onmessage" #(let [reader (t/reader :json)
                                payload (->> ^js % .-data (t/read reader))]
                            (treat-ws-message! state ws payload)))
    p))

#_
(.then (evaluate! ws state "cljs.user" "(throw (ex-info :foo {}))")
       #(prn :RES %))

#_
(.then (evaluate! ws state "cljs.user" "(prn :wow a)")
       #(prn :RES %))

#_
(.then (evaluate! ws state "cljs.user" "(+ 2 3)")
       #(prn :RES %))

#_
(send! ws {:op :runtime-print-sub
           :to 590})

#_
(.close ws)
