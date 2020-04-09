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

(defn- change-status [state status acc-str buffer-size]
  (swap! state #(-> %
                    (assoc :leaf-status status)
                    (update :acc-str str acc-str)
                    (update :buffer subs buffer-size))))

(defn- clear-status [state result]
  (swap! state #(-> %
                    (assoc :acc-str "" :leaf-status nil)
                    (update :acc conj result))))

(declare decode-sm)
(defn- decode-leaf [state fragment f]
  (let [status (:leaf-status @state)]
    (prn :DECODING-LEAF status @state)
    (cond
      (and (nil? status) (re-find #"\d" f))
      (let [[_ c term?] (re-find #"^(\d+)(:)" fragment)
            bytes (-> c js/parseInt delay)
            start (-> c count inc delay)]
        (if term?
          (do
            (prn :WAT? @start)
            (change-status state :s "" @start)
            (swap! state assoc :acc-bytes 0 :bytes-expected @bytes)
            (decode-sm state))
          []))

      (= :s status)
      (let [{:keys [bytes-expected acc-bytes]} @state
            cut-frag (.. Buffer (from fragment) (slice 0 (- bytes-expected acc-bytes)))
            _ (prn :CUT cut-frag)
            curr-bytes (+ acc-bytes (.-length cut-frag))
            cut-str (str cut-frag)]
        (swap! state #(-> %
                          (assoc :acc-bytes curr-bytes)
                          (update :acc-str str cut-str)
                          (update :buffer subs (count cut-str))))
        (prn :AND? curr-bytes bytes-expected)
        (when (= curr-bytes bytes-expected)
          (clear-status state (:acc-str @state)))
        (decode-sm state))

      (or (= status :i) (and (nil? status) (= "i" f)))
      (let [[res value term?] (re-find #"i?(\-?\d*)(e)?" fragment)]
        (change-status state :i value (count res))
        (when term?
          (clear-status state (js/parseInt (:acc-str @state))))
        (decode-sm state))

      :else
      (throw (ex-info "Garbage on parsing bencode" {:string fragment})))))

(defn- decode-sm [state]
  (let [level (:level @state)
        fragment (:buffer @state)
        f (first fragment)]
    (prn :DECODING @state)
    (cond
      (= "" fragment)
      (let [acc (:acc @state)]
        (prn :THE-END acc)
        (swap! state assoc :acc [])
        acc)

      (:leaf-status @state)
      (decode-leaf state fragment f)

      (#{"l" "m"} f)
      (do
        (swap! state #(-> %
                          (update :buffer subs 1)
                          (update :level inc)
                          (update :list-status conj (keyword f))
                          (assoc :list-acc [])
                          (update :buffer subs 1)))
        (recur state))

      (-> @state :list-status (get level) (= :l))
      (if (= "e" f)
        (let [result (decode-sm state)]
          (swap! state #(-> %
                            (update :list-acc conj result)
                            (update :buffer subs 1)))
          (:list-acc @state)))

      :else
      (decode-leaf state fragment f))))

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

(declare decode-one)
(defn- parse-list [state acc]
  (prn :PARSING-LIST acc @state)
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

(defn- remove-chars-and-continue [state char-size cont]
  (remove-chars state char-size)
  (cont state))

(defn- decode-one [state]
  (let [fragment (:buffer @state)
        f (first fragment)]
    (cond
      (= "i" f)
      (remove-chars-and-continue state 1 #(parse-int % ""))

      (= "l" f)
      (remove-chars-and-continue state 1 #(parse-list % []))

      :else
      (throw (ex-info "Garbage on parsing bencode" {:string fragment})))))

; Tries to use continuations to decode
(defn- decode-cont [state acc]
  (prn :DECODE-CONT :ACC acc :STATE @state)
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
      (println "\nAdding fragment" fragment)
      (swap! state update :buffer str fragment)
      (doto (decode-cont state [])
            (println "\n" :FINAL-STATE (pr-str @state))))))


(defn old-decoder
  []
  (let [state (atom "")]
    (fn [fragment]
      (swap! state str fragment)
      (let [[rest parsed] (decode-fragment @state [])]
        (reset! state rest)
        parsed))))

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
