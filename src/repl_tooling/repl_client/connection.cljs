(ns repl-tooling.repl-client.connection
  (:require [repl-tooling.editor-helpers :as helpers]
            [clojure.string :as str]
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

(defn- treat-new-state [control buffer new-state on-line on-fragment]
  (let [has-newline? #(re-find #"\n" (str %))
        [frags [last-line & rest]] (split-with (complement has-newline?) new-state)]

    (cond
      (= [:closed] new-state)
      (do (on-line nil) (on-fragment nil))

      (has-newline? last-line)
      (emit-line! control on-line on-fragment buffer frags last-line)

      (not-empty new-state)
      (schedule-fragment! control on-fragment buffer new-state))))

(defn treat-buffer! [buffer on-line on-fragment]
  (let [control (atom {:emitted-frags []})]
    (add-watch buffer :on-add #(treat-new-state control buffer %4 on-line on-fragment))))

(defn connect! [host port]
  (let [buffer (atom [])
        conn (doto (. net createConnection port host)
                   (.on "data" #(swap! buffer conj (str %)))
                   (.on "close" #(swap! buffer conj :closed)))]
                   ; (.write "[:repl-tooling$discover #?(:cljs :cljs :clj :clj :default :other)]\n"))]
    {:buffer buffer
     :conn conn}))

(comment
  (def res (connect! "localhost" 2030))

  (time
   (doseq [n (range 10)]
     (time (.write (:conn res) "(range 100000)\n"))))
  (time
   (-> res :buffer deref count))

  (-> res :buffer deref last)
  (-> res :buffer deref first)

  (.write (:conn res) ":cljs/quit\n")

  (reset! (:buffer res) []))
