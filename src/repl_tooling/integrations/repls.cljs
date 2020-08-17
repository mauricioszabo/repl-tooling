(ns repl-tooling.integrations.repls
  (:require [repl-tooling.repl-client.connection :as connection]
            [repl-tooling.repl-client.clj-helper :as h]
            [promesa.core :as p]
            [clojure.string :as str]
            [repl-tooling.nrepl.bencode :as bencode]
            [repl-tooling.repl-client.source :as source]
            [repl-tooling.repl-client.clojure :as clj]
            [repl-tooling.repl-client.nrepl :as nrepl]
            [repl-tooling.eval :as eval]))

;; Detection
(defn- detect-nrepl [buffer]
  (let [p (p/deferred)]
    (if (= [] @buffer)
      (add-watch buffer :nrepl
                 (fn [_ _ _ [val]]
                   (remove-watch buffer :nrepl)
                   (if (re-find #"^d\d" val)
                     (p/resolve! p true)
                     (p/resolve! p false))))
      (p/resolve! p false))
    p))

(defn- detect-output-kind [row kind-p]
  (when-let [row-kind (re-find #":using-(.*)-repl" (str row))]
    (p/resolve! kind-p (keyword (second row-kind)))))

(defn- detect-socket-kind [^js conn buffer]
  (let [kind-p (p/deferred)
        control (connection/treat-buffer! buffer #(detect-output-kind % kind-p) identity)]
    (p/let [_ (.write conn "\n") ; Flush nREPL data detection
            _ (connection/next-line control)
            _ (.write conn (str "#?("
                                ":bb :using-bb-repl "
                                ":joker :using-joker-repl "
                                ":clje :using-clje-repl "
                                ":cljs :using-cljs-repl "
                                ":cljr :using-cljr-repl "
                                ":clj :using-clj-repl "
                                ")\n"))
            _ (connection/next-line control)
            _ (.write conn ":using-unknown-repl\n")
            kind kind-p]
      (swap! control assoc :on-line identity)
      {:conn conn
       :buffer buffer
       :control control
       :repl-kind kind})))

(defn connect-and-detect! [host port on-output]
    (p/let [{:keys [conn buffer]} (connection/connect! host port)
            _ (p/delay 2)
            _ (.write conn (bencode/encode {:id "new-session" :op :clone}) "binary")
            nrepl? (detect-nrepl buffer)]
      (if nrepl?
        (nrepl/repl-for conn buffer on-output)
        (detect-socket-kind conn buffer))))

;; REPLs
(defn add-to-eval-queue [cmd-for command opts callback pending-evals eval-cmd]
  (let [id (or (:id opts) (gensym))
        command (cmd-for {:command command :id id})]
    (if-let [result (:result command)]
      (let [pending (assoc opts :command result :callback callback :id id)]
        (swap! pending-evals assoc id pending)
        (eval-cmd pending))
      (callback command))
    id))

(defrecord Generic [pending-evals cmd-for eval-cmd]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (add-to-eval-queue cmd-for command opts callback pending-evals eval-cmd))
  (break [_ _]))

;; Integrations, at last
(defn capture-eval-result [pending-evals on-output result]
  (let [[id edn-result] result
        {:keys [callback pass ignore]} (get @pending-evals id)
        msg (merge pass edn-result)]
    (when-not ignore (on-output msg))
    (callback msg)))

(defn- send-namespace [^js conn ns-command namespace control]
  (when namespace
    (swap! control update :ignore-output conj #"^\r?\n?.*?=> " #"(?:.+Namespace.+|nil)\r?\n")
    (.write conn (str "(" ns-command namespace ")"))))

(defn- wait-for-blob-done [kind conn control]
  (.write conn (cond-> (str (h/generic-blob-contents) "\n")
                       (= :joker kind) (str/replace #"clojure.string" "joker.string")))
  (p/loop [curr (connection/next-line control)]
    (when-not (re-find #":DONE-BLOB" curr)
      (p/recur (connection/next-line control)))))

(defn- instantiate-correct-evaluator [repl-kind ^js conn control on-output]
  (let [pending-evals (atom {})
        cmd-for (case repl-kind
                  :bb (fn [{:keys [command id]}]
                        (source/wrap-command id command 'java.lang.Throwable true))
                  :joker (fn [{:keys [command id]}]
                           (let [o (source/wrap-command id command 'Error false)
                                 res (:result o)]
                             (if res
                               {:result (str/replace res #"clojure\.core/" "joker.core/")}
                               o)))
                  :cljs (fn [{:keys [command id]}]
                          (source/wrap-command id command :default true))
                  :cljr (fn [{:keys [command id]}]
                          (source/wrap-command id command 'System.Exception true))
                  :clje (fn [{:keys [command id]}]
                          (source/wrap-command id command '_ false))
                  (fn [{:keys [command id]}]
                    (source/wrap-command id command 'Exception true)))
        eval-command (case repl-kind
                       :bb (fn [{:keys [id command namespace]}]
                             (send-namespace conn "in-ns '" namespace control)
                             (swap! control update :pending-evals conj id)
                             (.write conn command))
                       :joker (fn [{:keys [id command namespace]}]
                                (send-namespace conn "joker.core/ns " namespace control)
                                (swap! control update :pending-evals conj id)
                                (.write conn command))
                       (fn [{:keys [id command namespace]}]
                         (send-namespace conn "in-ns '" namespace control)
                         (swap! control update :pending-evals conj id)
                         (.write conn command)))]

    (if (= :clj repl-kind)
      (clj/prepare-unrepl-evaluator conn control on-output)
      (p/do!
        (p/race [(wait-for-blob-done repl-kind conn control) (p/delay 1000)])
        (swap! control assoc :ignore-prompt true)
        (connection/prepare-evals control
                                  #(if-let [out %] (on-output {:out out}) (on-output nil))
                                  #(capture-eval-result pending-evals on-output %))
        (->Generic pending-evals cmd-for eval-command)))))

(defn- ignore-output-on-control [control repl-kind]
  (if-not (= :unknown repl-kind)
    (swap! control update :ignore-output conj #":using-unknown-repl" #"^\r?\n?.*?=> ")))

(defonce connections (atom {}))
(defn connect-repl! [id host port on-output]
  (p/let [{:keys [conn control repl-kind buffer evaluator]}
          (connect-and-detect! host port on-output)]

    (swap! connections assoc id {:conn conn :buffer buffer})
    (if evaluator
      [repl-kind evaluator]
      (p/let [_ (ignore-output-on-control control repl-kind)
              evaluator (instantiate-correct-evaluator repl-kind conn control on-output)]
        [repl-kind evaluator]))))

(defn disconnect! [id]
  (when-let [{:keys [conn buffer]} ^js (get @connections id)]
    (js/setTimeout (fn []
                     (when-not (-> @buffer last (= :closed))
                       (swap! buffer conj :closed)))
                   1000)
    (.end conn)))
