(ns repl-tooling.nrepl.bencode
  (:require ["buffer" :refer [Buffer]]
            [clojure.string :as str]))

(defn encode [this]
  (cond
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

(defn- decode-fragment [fragment acc]
  (let [f (-> fragment first str)]
    (println (= "i" f) fragment acc)
    (cond
      (= "i" f)
      (if-let [[res value] (re-find #"i(\-?\d+)e" fragment)]
        (recur (subs fragment (count res)) (conj acc (js/parseInt value)))
        [fragment acc])

      (= "l" f)
      (let [[rest inner] (decode-fragment (subs fragment 1) [])]
        (if (= "e" (first rest))
          (recur (subs rest 1) (conj acc inner))
          [fragment acc]))

      (= "d" f)
      (let [[rest inner] (decode-fragment (subs fragment 1) [])]
        (if (= "e" (first rest))
          (recur (subs rest 1) (conj acc (apply hash-map inner)))
          [fragment acc]))

      (re-find #"\d" f)
      (let [[_ c] (re-find #"^(\d+):" fragment)
            re (re-pattern (str "^\\d*:(.{" c "})"))
            [res value] (re-find re fragment)]
        (if value
          (recur (subs fragment (count res)) (conj acc value))
          [fragment acc]))

      :else
      [fragment acc])))

(defn decoder [callback]
  (let [state (atom "")]
    (fn [fragment]
      (swap! state str fragment)
      (println)
      (let [[rest parsed] (decode-fragment @state [])]
        (reset! state rest)
        (prn :PARSED parsed)
        (doseq [p parsed] (callback p))))))

; (re-find #"^l(([^fooe])*?)e" "li0ei2eele")

#_
(re-find #"i(\d+)e" "i200ei10e")
;
; (drop 1 "foobar")
; (subs ".{}" 2)
