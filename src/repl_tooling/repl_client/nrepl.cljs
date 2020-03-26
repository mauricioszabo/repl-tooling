(ns repl-tooling.repl-client.nrepl
  ; (:require-macros [repl-tooling.repl-client.clj-helper :refer [blob-contents]])
  (:require [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [promesa.core :as p]
            ["nrepl-client" :as nrepl]))

(def repl (js/require "nrepl-client"))

(defn- connect-socket! [host port]
  (let [p (p/deferred)
        client (. repl connect #js {:host host :port port})]
    (.on client "connect" #(p/resolve! p client))
    p))

(defrecord Evaluator [^js client pending session-id]
  eval/Evaluator
  (evaluate [this command opts callback]
    (when-let [namespace (:namespace opts)]
      (.send client (str "(in-ns '" namespace ")") identity))

    (let [id (:id opts (gensym "eval"))
          op {:op "eval"
              :code (str command)
              :id id
              :session session-id}
          full-op (cond-> op
                          (:filename opts) (assoc :file (:filename opts))
                          (:col opts) (assoc :column (:col opts))
                          (:row opts) (assoc :line (:row opts)))]
      (swap! pending assoc (str id) {:callback callback})
      (.send client (clj->js full-op))
      id))

  (break [_ _]
    (.interrupt client session-id)))

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
    (on-output {:out out})))

(defn connect! [host port on-output]
  (p/let [pending (atom {})
          client (connect-socket! host port)
          sess (js/Promise. (fn [resolve] (.clone client #(resolve (js->clj %2)))))
          session (-> sess first (get "new-session"))]
    (.on ^js client "close" #(on-output nil))
    (.. ^js client
        -messageStream
        (on "messageSequence" #(doseq [msg (js->clj %2)]
                                 (treat-output! pending on-output msg))))
    (->Evaluator client pending session)))

#_
(.end (:client client))
#_
(.then (connect! "localhost" 1337 #(prn :OUT %)) #(def client %))
#_
(.then (connect! "localhost" 46565 #(prn :OUT %)) #(def client %))
#_
(eval/eval client "(/ 10 0)")
