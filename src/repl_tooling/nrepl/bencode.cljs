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

(defn old-decoder
  []
  (let [state (atom "")]
    (fn [fragment]
      (swap! state str fragment)
      (let [[rest parsed] (decode-fragment @state [])]
        (reset! state rest)
        parsed))))

(defn- remove-chars [state char-size]
  (swap! state update :buffer subs char-size))

(defn- parse-int [state fragment]
  (let [buffer (:buffer @state)
        total (str fragment buffer)
        [res number term?] (re-find #"(\-?\d*)(e)?" total)]
    (cond
      term?
      (do
        (->> buffer (re-find #"\-?\d*e") count (remove-chars state))
        (js/parseInt number))

      res
      (do
        (remove-chars state (count buffer))
        #(parse-int state total))

      :else
      (throw (ex-info "Error parsing INT" {:string total})))))

(defn- extract-string
  "Extracts string. Accumulates in acc-str, counts how many bytes are accumulated (on
acc-bytes), until the total (bytes-expected) is fulfilled"
  [state acc-str bytes-expected acc-bytes]
  (let [fragment (:buffer @state)
        cut-frag (.. Buffer (from fragment) (slice 0 (- bytes-expected acc-bytes)))
        curr-bytes (+ acc-bytes (.-length cut-frag))
        cut-str (str cut-frag)]
    (remove-chars state (count cut-str))
    (if (= curr-bytes bytes-expected)
      (str acc-str cut-str)
      #(extract-string state (str acc-str cut-str) bytes-expected curr-bytes))))

(defn- parse-str [state fragment]
  (let [buffer (:buffer @state)
        total (str fragment buffer)
        [res number term?] (re-find #"^(\d+)(:)?" total)]
    (->> buffer (re-find #"\d*:?") count (remove-chars state))
    (if term?
      (extract-string state "" (js/parseInt number) 0)
      #(parse-str state total))))

(declare decode-one)
(defn- parse-list [state acc]
  (let [inner (fn decode-inner [result]
                (if (fn? result)
                  #(decode-inner (result))
                  (parse-list state (conj acc result))))]
    (cond
      (-> @state :buffer first (= "e"))
      (do (remove-chars state 1) acc)

      (-> @state :buffer (= ""))
      #(parse-list state acc)

      :else
      (inner (decode-one state)))))

(defn- parse-map [state acc]
  (let [inner (fn decode-inner [result]
                (if (fn? result)
                  #(decode-inner (result))
                  (apply hash-map result)))]
    (inner (parse-list state acc))))

(defn- remove-chars-and-continue [state char-size cont]
  (remove-chars state char-size)
  (cont state))

(defn- decode-one [state]
  (let [fragment (:buffer @state)
        f (str (first fragment))]
    (cond
      (= "i" f)
      (remove-chars-and-continue state 1 #(parse-int % ""))

      (re-find #"\d" f)
      (parse-str state "")

      (= "l" f)
      (remove-chars-and-continue state 1 #(parse-list % []))

      (= "d" f)
      (remove-chars-and-continue state 1 #(parse-map % []))

      :else
      (throw (ex-info "Garbage on parsing bencode" {:string fragment})))))

; Tries to use continuations to decode
(defn- decode-cont [state acc]
  (if (= "" (:buffer @state))
    acc
    (let [cont (:cont @state)
          fragment (:buffer @state)
          f (first fragment)
          res (if cont
                (cont)
                (decode-one state))]

      (if (fn? res)
        (do
          (swap! state assoc :cont res)
          [])
        (do
          (swap! state assoc :cont nil)
          (recur state (conj acc res)))))))

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
  (let [state (atom {:buffer "" :cont nil})]
    (fn [fragment]
      (swap! state update :buffer str fragment)
      (decode-cont state []))))

;; Performance tests
#_
(->> 100000 range pr-str encode (def encoded))
#_
(time
 (let [decode! (decoder)]
   (->> encoded
        (partition-all 20 20)
        (map #(apply str %))
        (map decode!)
        last
        last
        last)))
#_
(time
 (let [decode! (old-decoder)]
   (->> encoded
        (partition-all 20 20)
        (map #(apply str %))
        (map decode!)
        last
        last
        last)))
