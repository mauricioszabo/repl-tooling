(ns repl-tooling.repl-client.connection
  (:require [repl-tooling.editor-helpers :as helpers]
            ["net" :as net]))

(defn connect! [host port]
  (let [buffer (atom [])
        conn (doto (. net createConnection port host)
                   (.on "data" #(swap! buffer conj [(-> js/Date new long) (str %)]))
                   (.on "close" #(swap! buffer conj [(-> js/Date new long) :closed])))]
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
