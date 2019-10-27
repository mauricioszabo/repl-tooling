(ns repl-tooling.integrations.repls
  (:require [repl-tooling.repl-client.connection :as connection]
            [clojure.reader :as edn]
            [clojure.core.async :include-macros true :as async]
            [rewrite-clj.node :as node]
            [rewrite-clj.parser :as parser]
            [repl-tooling.eval :as eval]))

(declare normalize-command)
(defn- conv-node [node]
  (cond
    (or (node/whitespace-or-comment? node)
        (node/linebreak? node))
    (node/whitespace-node " ")

    :else
    (normalize-command node)))

(defn- normalize-command [command]
  (cond-> command (contains? command :children) (update :children #(map conv-node %))))

(defn parse-command [command remove-lines?]
  (let [cmd (try
              {:result (parser/parse-string command)}
              (catch :default e
                {:error (.-message e)}))]
    (if-let [res (:result cmd)]
      (if (= (str res) command)
        {:result (str (cond-> res remove-lines? normalize-command))}
        {:error "Unexpected Token."})
      cmd)))

;; Detection

(defn- detect-output-kind [row chan]
  (when-let [row-kind (re-find #":using-(.*)-repl" row)]
    (async/put! chan (keyword (second row-kind)))))

(defn connect-and-detect! [host port]
  (let [{:keys [conn buffer] :as a} (connection/connect! host port)
        kind-chan (async/promise-chan)]
    (.write conn "\n")
    (.write conn "#?(:cljs :using-cljs-repl :clj :using-clj-repl :bb :using-bb-repl)\n")
    (.write conn ":using-unknown-repl\n")
    {:conn conn
     :control (connection/treat-buffer! buffer #(detect-output-kind % kind-chan) identity)
     :repl-kind (js/Promise. (fn [resolve] (-> kind-chan async/<! resolve async/go)))}))

;; REPLs
(defn add-to-eval-queue [conn command opts callback pending-evals eval-cmd]
  (let [command (parse-command command true)
        id (or (:id opts) (gensym))]
    (if-let [result (:result command)]
      (let [pending (assoc opts :command result :callback callback :id id)]
        (swap! pending-evals assoc id pending)
        (eval-cmd conn pending))
      (callback command))
    id))

(defrecord Generic [conn pending-evals eval-cmd]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (add-to-eval-queue conn command opts callback pending-evals eval-cmd))
  (break [_ _]))

;; Integrations, at last
(defn capture-eval-result [pending-evals result]
  (let [[id edn-string] result
        {:keys [callback]} (get @pending-evals id)]
    (callback edn-string)))

(defn- send-command! [^js conn id command control ex-type]
  (let [command (str "(try (clojure.core/let [res (do " command ")]"
                     "['tooling$eval-res '" id " {:result (clojure.core/pr-str res)}]) "
                     "(catch " ex-type " e "
                    "['tooling$eval-res '" id " {:error (clojure.core/pr-str e)}]))\n")]
    (swap! control update :pending-evals conj id)
    (.write conn command)))

(defn- instantiate-correct-evaluator [repl-kind conn control on-output]
  (let [pending-evals (atom {})
        eval-command (case repl-kind
                       :bb (fn [^js conn {:keys [command id] :as opts}]
                             (send-command! conn id command control "Exception"))
                       :cljs (fn [^js conn {:keys [command namespace id]}]
                               (when namespace (.write conn (str "(in-ns " namespace ")")))
                               (send-command! conn id command control ":default"))
                       (fn [^js conn {:keys [command namespace id]}]
                         (when namespace (.write conn (str "(in-ns " namespace ")")))
                         (send-command! conn id command control "Throwable")))]

    (when-not (= repl-kind :clj)
      (swap! control assoc :ignore-prompt true))

    (connection/prepare-evals control on-output #(capture-eval-result pending-evals %))
    (->Generic conn pending-evals eval-command)))

(defonce connections (atom {}))
(defn connect-repl! [id host port on-output]
  (let [{:keys [conn control repl-kind]} (connect-and-detect! host port)]
    (swap! connections assoc id conn)
    (.then ^js repl-kind #(instantiate-correct-evaluator % conn control on-output))))

(comment
  (. (connect-repl! :bb "localhost" 2211 #(prn :OUTPUT %)) then #(def evaluator %))
  (disconnect! :bb)
  (eval/evaluate evaluator "(/ 20 2)" {} #(prn :RESULT %)))

(defn disconnect! [id]
  (when-let [conn ^js (get @connections id)]
    (.end conn)))
