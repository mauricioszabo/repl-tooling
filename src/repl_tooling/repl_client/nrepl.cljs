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
      id))

  (break [_ _]
    (.write conn (bencode/encode {:op :interrupt :session session-id}) "binary")))

(defn- treat-output! [pending on-output msg]
  (when-let [value (get msg "value")]
    (when-let [{:keys [callback]} (get @pending (get msg "id"))]
      (callback {:result value :as-text value})
      (swap! pending dissoc)))

  (when-let [value (get msg "ex")]
    (when-let [{:keys [callback]} (get @pending (get msg "id"))]
      (let [value (->> value (tagged-literal 'repl-tooling/literal-render) pr-str)]
        (callback {:error value :as-text value})
        (swap! pending dissoc))))

  (when (some #{"interrupted"} (get msg "status"))
    (let [{:keys [callback]} (get @pending (get msg "id"))]
      (when callback (callback {:error "Interrupted!" :as-text "Interrupted!"}))))

  (when-let [out (get msg "out")]
    (on-output {:out out}))
  (when-let [out (get msg "err")]
    (on-output {:err out})))

(defn- treat-socket-output! [{:keys [decode! buffer val treat on-output]}]
  (when (= :closed val)
    (on-output nil))
  (when val
    (swap! buffer subvec 1)
    (doseq [result (decode! val)]
      (treat result))))

(def ^:private detection (str "#?("
                              ":bb :bb "
                              ":joker :joker "
                              ":clje :clje "
                              ":cljs :cljs "
                              ":cljr :cljr "
                              ":clj :clj "
                              ":default :unknown"
                              ")"))

(defn- callback-fns []
  (let [calls (atom #{})]
    [calls
     (fn [arg]
       (doseq [call @calls]
         (call arg)))]))

(defn- wait-for [calls id timeout]
  (let [p (p/deferred)
        f (fn [msg]
            (when (= id (get msg "id"))
              (p/resolve! p msg)))]
    (swap! calls conj f)
    (p/do!
     (p/delay timeout)
     (swap! calls disj f)
     (p/reject! p :timeout))
    p))

(defn- session-for [buffer on-output]
  (let [decode! (bencode/decoder)
        pending (atom {})
        new-out (fn [out]
                  (on-output out)
                  (when (nil? out) (remove-watch buffer :nrepl-evaluator)))
        [calls callback] (callback-fns)
        session-msg (wait-for calls "new-session" 1000)]
    (swap! calls conj #(treat-output! pending new-out %))
    (add-watch buffer :nrepl-evaluator
               (fn [_ _ _ [val]]
                 (treat-socket-output! {:decode! decode!
                                        :buffer buffer
                                        :val val
                                        :treat callback
                                        :on-output new-out})))
    (swap! buffer identity)
    (p/then session-msg
            #(vector pending %))))

(defn repl-for [^js conn buffer on-output]
  (p/let [[pending session-msg] (session-for buffer on-output)
          session-id (get session-msg "new-session")
          evaluator (->Evaluator conn pending session-id)
          repl-kind (-> (eval/eval evaluator detection)
                        (p/then :result)
                        (p/catch (constantly :unknown)))]
    {:evaluator evaluator
     :conn conn
     :buffer buffer
     :repl-kind repl-kind}))
