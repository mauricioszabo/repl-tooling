(ns repl-tooling.repl-client.lumo
  (:require [repl-tooling.repl-client.protocols :as repl]
            [repl-tooling.repl-client :as client]
            [cljs.core.async :refer [chan <! >!] :refer-macros [go-loop go]]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(defn- parse-output [output]
  (let [parsed (some-> output not-empty pop)
        out (str/join "\n" parsed)]
    (println "PARSED!!!!")
    (prn parsed)
    (println "--------------")
    (prn output)
    (println "--------------")

    (if (re-find  #"^\[\w__\d" out)
      (let [[_ out res] (reader/read-string out)]
        {:result res :out out})
      {:out out})))

(def ^:private buffer-txt (atom []))
(defn- code-to-lumo [identifier code]
  (let [reader (str (gensym) "reader" (gensym))
        result (str (gensym) "result" (gensym))]
    (str "(let [" reader " (goog.string/StringBuffer.)]
            (binding [cljs.core/*print-newline* true
                      cljs.core/*print-fn* (fn [x] (.append " reader " x))]
        (let [" result " (cljs.core/eval '" code ")]
          ['" identifier "
           (cljs.core/str " reader ")
           " result "])))")))

(defrecord Lumo [in out socket]
  repl/Repl
  (treat-data [_ data]
    (let [string (str data)]
      (println "DATA" string)
      (swap! buffer-txt #(vec (concat (some-> % not-empty pop)
                                      (str/split (str (last %) string) #"\n"))))
      (when (str/ends-with? string "=> ")
        (let [output (parse-output @buffer-txt)]
          (go (>! out output))
          (reset! buffer-txt [])))))

  (send-command [_ command]
    (let [sym (gensym)]
      (println "SENDING")
      (println (code-to-lumo sym command))
      (.write socket (code-to-lumo sym command)))))
                ; (let [code (
                ;             (reader/read-string))])))

(defn connect-socket! [session-name host port]
  (let [[in out socket] (client/socket! session-name host port)
        repl (->Lumo in out socket)]
    (client/integrate-repl in repl socket)
    [in out]))
