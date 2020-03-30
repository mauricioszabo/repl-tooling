(ns repl-tooling.repl-client.nrepl
  (:require [repl-tooling.nrepl.bencode :as bencode]
            [repl-tooling.eval :as eval]
            [promesa.core :as p]))

(defrecord Evaluator [^js conn pending session-id]
  eval/Evaluator
  (evaluate [this command opts callback]
    (let [id (:id opts (gensym "eval"))
          op {:op "eval"
              :code (str command)
              :id id
              :session session-id}
          full-op (cond-> op
                          (:namespace opts) (assoc :ns (:namespace opts))
                          (:filename opts) (assoc :file (:filename opts))
                          (:col opts) (assoc :column (:col opts))
                          (:row opts) (assoc :line (:row opts)))]
      (swap! pending assoc (str id) {:callback callback})
      (.write conn (bencode/encode full-op) "binary")
      id)))

(defn- treat-output! [pending on-output msg]
  (when-let [value (get msg "value" (get msg "ex"))]
    (let [{:keys [callback]} (get @pending (get msg "id"))
          key (if (contains? msg "value") :result :error)]
      (when callback
        (callback {key value :as-text value})
        (swap! pending dissoc))))

  (when (some #{"interrupted"} (get msg "status"))
    (let [{:keys [callback]} (get @pending (get msg "id"))]
      (when callback (callback {:error "Interrupted!" :as-text "Interrupted!"}))))

  (when-let [out (get msg "out")]
    (on-output {:out out}))
  (when-let [out (get msg "err")]
    (on-output {:err out})))

(defn- treat-socket-output! [{:keys [pending decode! buffer val on-output]}]
  (when (= :closed val)
    (on-output nil))
  (when val
    (swap! buffer subvec 1)
    (doseq [result (decode! val)]
      (treat-output! pending on-output result))))

(defn- capture-session-id! [buffer]
  (let [decode! (bencode/decoder)
        p (p/deferred)]
    (add-watch buffer :nrepl-evaluator-session
               (fn [_ _ _ [val]]
                 (when val
                   (swap! buffer subvec 1)
                   (doseq [result (decode! val)]
                     (when-let [session-id (get result "new-session")]
                       (remove-watch buffer :nrepl-evaluator-session)
                       (p/resolve! p session-id))))))
    p))

(defn repl-for [^js conn buffer on-output]
  (p/let [decode! (bencode/decoder)
          pending (atom {})
          new-out (fn [out]
                    (on-output out)
                    (when (nil? out) (remove-watch buffer :nrepl-evaluator)))
          _ (.write conn (bencode/encode {:op :clone}) "binary")
          session-id (capture-session-id! buffer)]
    (add-watch buffer :nrepl-evaluator
               (fn [_ _ _ [val]]
                 (treat-socket-output! {:decode! decode!
                                        :buffer buffer
                                        :val val
                                        :pending pending
                                        :on-output new-out})))
    {:evaluator (->Evaluator conn pending session-id)
     :conn conn
     :buffer buffer
     :repl-kind :clj}))
