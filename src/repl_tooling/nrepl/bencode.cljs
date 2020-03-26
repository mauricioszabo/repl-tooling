(ns repl-tooling.nrepl.bencode
  (:require ["buffer" :refer [Buffer]]
            [clojure.string :as str]))

(defn encode [this]
  (cond
    (boolean? this) (if this "1" "0")
    (number? this) (str "i" this "e")
    (string? this) (str (.byteLength Buffer this) ":" this)
    (keyword? this) (-> this str (str/replace-first #":" "") encode)
    (symbol? this) (-> this str encode)
    (map? this) (str "d"
                     (->> this
                          (map (fn [[k v]] (str (encode k) (encode v))))
                          (str/join ""))
                     "e")
    (coll? this) (str "l" (->> this (map encode) (str/join "")) "e")
    :else (throw (ex-info "Can't encode this object" {:object this
                                                      :type (type this)}))))

(defn decoder [callback]
  (let [state (atom "")]
    (fn [fragment]
      (swap! state str fragment)
      (let [frags @state]
        (println "")
        (prn :FRAGS frags)
        (prn :state @state)
        (cond
          (re-find #"^\d+?\:" frags)
          (let [[_ c] (re-find #"^(\d+):" frags)
                re (re-pattern (str "^\\d+:(.{" c "})"))]
            (when-let [[res value] (re-find re frags)]
              (swap! state subs (count res))
              (callback value)
              (recur "")))

          (re-find #"^[01][^:]" frags)
          (do
            (swap! state subs 1)
            (callback (= "1" (first frags)))
            (recur ""))

          (re-find #"^i" frags)
          (when-let [[res value] (re-find #"i(\d+)e" frags)]
            (swap! state subs (count res))
            (callback (js/parseInt value))
            (recur "")))))))

#_
(re-find #"i(\d+)e" "i200ei10e")
;
; (drop 1 "foobar")
; (subs ".{}" 2)
