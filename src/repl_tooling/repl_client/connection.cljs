(ns repl-tooling.repl-client.connection
  (:require [repl-tooling.editor-helpers :as helpers]
            [clojure.string :as str]
            [clojure.reader :as edn]
            [rewrite-clj.parser :as parser]
            ["net" :as net]))

(defn- emit-line! [control on-line on-fragment buffer frags last-line]
  (js/clearTimeout (:timeout-id @control))
  (let [[fst snd] (str/split last-line #"\n" 2)]
    (on-line (apply str (concat (:emitted-frags @control) frags [fst])))
    (on-fragment (apply str (concat frags [fst "\n"])))
    (swap! buffer #(if (empty? snd)
                     (subvec % (-> frags count inc))
                     (-> % (subvec (count frags)) (assoc 0 snd))))))

(defn- schedule-fragment! [control on-fragment buffer new-state]
  (let [frags (cond-> new-state (-> new-state peek (= :closed)) pop)]
    (js/clearTimeout (:timeout-id @control))
    (swap! control assoc
           :timeout-id
           (js/setTimeout
            (fn []
              (on-fragment (apply str frags))
              (swap! control update :emitted-frags into frags)
              (swap! buffer #(subvec % (count frags))))
            1000))))

(defn- treat-new-state [control buffer new-state]
  (let [has-newline? #(re-find #"\n" (str %))
        {:keys [on-line on-fragment]} @control
        [frags [last-line & rest]] (split-with (complement has-newline?) new-state)]
    (cond
      (= [:closed] new-state)
      (do
        (remove-watch buffer :on-add)
        (on-fragment nil)
        (on-line nil))

      (has-newline? last-line)
      (emit-line! control on-line on-fragment buffer frags last-line)

      (not-empty new-state)
      (schedule-fragment! control on-fragment buffer new-state))))

(defn treat-buffer! [buffer on-line on-fragment]
  (let [control (atom {:emitted-frags []
                       :on-line on-line
                       :on-fragment on-fragment})]
    (add-watch buffer :on-add #(treat-new-state control buffer %4))
    control))

(defn- calculate-match [output control]
  (when-let [re (-> @control :ignore-output first)]
    (if-let [match (re-find re output)]
      match
      (do
        (swap! control update :ignore-output rest)
        (recur output control)))))

(defn- send-output [output control on-output]
  (when (not-empty output)
    (if-let [match (calculate-match output control)]
      (let [new-output (subs output (count match))]
        (send-output new-output control on-output))
      (on-output output))))

(defn- parse-edn-string [output control]
  (try
    (str (parser/parse-string output))
    (catch :default ex
      (when (re-find #"^Unexpected EOF" (.-message ex))
        (swap! control assoc :incomplete-result output)
        "[nil :ignore]"))))

(defn- send-result [output control on-output on-result]
  (let [edn-str (parse-edn-string output control)
        [_ id res] (edn/read-string edn-str)
        exist? (:pending-evals @control)]
    (when-not (= :ignore id)
      (swap! control dissoc :incomplete-result)
      (if (exist? id)
        (let [edn-size (count edn-str)]
          (swap! control update :pending-evals disj id)
          (when (:ignore-prompt @control) (swap! control update :ignore-output
                                                 conj #"^\n?.*?=> " #"\n"))
          (on-result [id res])
          (send-output (subs output edn-size) control on-output))
        (send-output output control on-output)))))

(defn- treat-output [output control on-output on-result]
  (if output
    (let [new-output (str (:incomplete-result @control) output)
          idx (some-> #"\[tooling\$eval-res" (.exec new-output) .-index)]
      (case idx
        nil (send-output new-output control on-output)
        0 (send-result new-output control on-output on-result)
        (do
          (send-output (subs new-output 0 idx) control on-output)
          (send-result (subs new-output idx) control on-output on-result))))
    (on-output nil)))

(defn prepare-evals [control on-output on-result]
  (swap! control assoc
         :pending-evals #{}
         :on-fragment #(treat-output % control on-output on-result)))

(defn connect! [host port]
  (let [buffer (atom [])
        conn (doto (. net createConnection port host)
                   (.on "data" #(swap! buffer conj (str %)))
                   (.on "close" #(swap! buffer conj :closed)))]
    {:buffer buffer
     :conn conn}))
