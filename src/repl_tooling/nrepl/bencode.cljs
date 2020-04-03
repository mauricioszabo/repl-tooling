(ns repl-tooling.nrepl.bencode
  (:require ["buffer" :refer [Buffer]]
            [clojure.string :as str]))

(defn encode
  "Encodes a map/vector/string/number into BEncode format"
  [this]
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
            chars (js/parseInt c)
            start (-> c count inc)
            value (str (.slice (.from js/Buffer fragment) start (+ start chars)))]
        (if (->> value (.byteLength js/Buffer) (= chars))
          (recur (subs fragment (-> value count (+ start))) (conj acc value))
          [fragment acc]))

      :else
      [fragment acc])))

(defn decoder
  "Starts a stateful decoder. It will return a function that accepts one parameter
(a string) and it'll try to decode it as a BEncode value. It'll return the BEncode
structures it finds, or an empty vector if it didn't found anything.

Ex:
(let [decode! (decoder)]
  (is (= [10] (decode! \"i10e\")))
  (is (= [] (decode! \"i1\")))
  (is (= [10] (decode! \"0e\"))))"
  []
  (let [state (atom "")]
    (fn [fragment]
      (swap! state str fragment)
      (let [[rest parsed] (decode-fragment @state [])]
        (reset! state rest)
        parsed))))
