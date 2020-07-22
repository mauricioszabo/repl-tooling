(ns repl-tooling.repl-client.connection
  (:require [clojure.string :as str]
            [clojure.reader :as edn]
            [rewrite-clj.parser :as parser]
            [promesa.core :as p]
            ["net" :as net]))

(defn- emit-line! [control on-line on-fragment buffer frags last-line]
  (js/clearTimeout (:timeout-id @control))
  (let [raw-lines (vec (.split last-line #"\r?\n"))
        last-fragment (peek raw-lines)
        lines (-> raw-lines
                  (update 0 #(apply str (concat (:emitted-frags @control) frags [%])))
                  butlast)
        [first-line & rest-of-lines] lines]
    (on-line first-line)
    (on-fragment (apply str (concat frags [(first raw-lines) "\n"])))
    (doseq [line rest-of-lines]
      (on-line line)
      (on-fragment (str line "\n")))
    (when-let [p (:next-line-prom @control)]
      (p/resolve! p first-line)
      (swap! control dissoc :next-line-prom))

    (swap! buffer #(if (empty? last-fragment)
                     (subvec % 1)
                     (-> %
                         (subvec (count frags))
                         (assoc 0 last-fragment))))))

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
  ; (prn :STR new-state)
  (let [has-newline? #(re-find #"\r?\n" (str %))
        {:keys [on-line on-fragment]} @control
        [frags [last-line]] (split-with (complement has-newline?) new-state)]
    ; (prn :SPLIT (split-with (complement has-newline?) new-state))
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

(defn next-line [control]
  (let [p (p/deferred)]
    (swap! control assoc :next-line-prom p)
    p))

(defn- calculate-match [output control]
  (when-let [re (-> @control :ignore-output first)]
    (swap! control update :ignore-output rest)
    (if-let [match (re-find re output)]
      match
      (recur output control))))

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
                                                 conj #"^\r?\n?.*?=> " #"\r?\n"))
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
  (js/Promise.
   (fn [resolve fail]
     (let [buffer (atom [])
           conn (. net createConnection port host)]
       (.on conn "connect" #(resolve {:buffer buffer :conn conn}))
       (.on conn "data" #(swap! buffer conj (str %)))
       (.on conn "error" #(fail (. ^js % -errno)))
       (.on conn "close" #(swap! buffer conj :closed))))))
