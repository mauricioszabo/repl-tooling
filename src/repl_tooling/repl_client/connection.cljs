(ns repl-tooling.repl-client.connection
  (:require [repl-tooling.editor-helpers :as helpers]
            ["net" :as net]))

; (def connection (doto (. net createConnection 4444 "localhost")
;                       (.on "data" #(prn (str %)))
;                       (.on "drain" #(prn :drained))
;                       (.on "close" #(prn :DOPE))
;                       (.write "[:repl-tooling$discover #?(:cljs :cljs :clj :clj :default :other)]\n")))
;
; (.end connection)
; (.write connection
;         "[:repl-tooling$discover #?(:cljs :cljs :clj\n   :clj\n:default :other)]\n")
;
; (.write connection "(prn :LOL)")
; (.-flush connection)
; 
(defn connect [host port]
  (.-createConnection net))
