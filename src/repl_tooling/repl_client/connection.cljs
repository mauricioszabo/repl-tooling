(ns repl-tooling.repl-client.connection
  (:require [repl-tooling.editor-helpers :as helpers]
            [clojure.string :as str]
            ["net" :as net]))

(defn- emit-line! [on-line buffer frags last-line]
  (let [[fst snd] (str/split last-line #"\n" 2)]
    (on-line (apply str (concat frags [fst])))
    (swap! buffer #(-> %
                       (subvec (count frags))
                       (assoc 0 snd)))))

(defn- treat-new-state [buffer new-state on-line on-fragment]
  (let [has-newline? #(re-find #"\n" (str %))
        [frags [last-line & rest]] (split-with (complement has-newline?) new-state)]
    (if (has-newline? last-line)
      (emit-line! on-line buffer frags last-line))))

(defn treat-buffer! [buffer on-line on-fragment]
  (add-watch buffer :on-add #(treat-new-state buffer %4 on-line on-fragment)))

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
