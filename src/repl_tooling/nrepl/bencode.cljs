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
                    (update :status conj status)
                    (update :acc-str str acc-str)
                    (update :buffer subs buffer-size))))

(defn- clear-status [state result]
  (swap! state #(-> %
                    (assoc :acc-str "")
                    (update :status empty)
                    (update :acc conj result))))

(defn- decode-sm [state]
  (swap! state assoc :acc [])
  (loop []
    ; (prn :STATE @state)
    (let [fragment (:buffer @state)
          status (-> @state :status first)
          f (first fragment)]
      (cond
        (= "" fragment)
        (let [acc (:acc @state)]
          (swap! state assoc :acc [])
          acc)

        (and (nil? status) (re-find #"\d" f))
        (let [[_ c term?] (re-find #"^(\d+)(:)" fragment)
              bytes (-> c js/parseInt delay)
              start (-> c count inc delay)]
          ; (prn :TERM? term?)
          (if term?
            (do
              (change-status state :s "" @start)
              (swap! state assoc :acc-bytes 0 :bytes-expected @bytes)
              (recur))
            []))

        (= :s status)
        (let [{:keys [bytes-expected acc-bytes]} @state
              cut-frag (.. Buffer (from fragment) (slice 0 (- bytes-expected acc-bytes)))
              curr-bytes (+ acc-bytes (.-length cut-frag))
              cut-str (str cut-frag)]
          ; (prn [:cut-frag acc-bytes curr-bytes])
          (when (> curr-bytes bytes-expected)
            (throw (ex-info "WOW, something is REALLY WRONG HERE!" {:acc curr-bytes})))
          (swap! state #(-> %
                            (assoc :acc-bytes curr-bytes)
                            (update :acc-str str cut-str)
                            (update :buffer subs (count cut-str))))
          (when (= curr-bytes bytes-expected)
            (clear-status state (:acc-str @state)))
          (recur))

        (or (= status :i) (and (nil? status) (= "i" f)))
        (let [[res value term?] (re-find #"i?(\-?\d*)(e)?" fragment)]
          (change-status state :i value (count res))
          (when term?
            (clear-status state (js/parseInt (:acc-str @state))))
          (recur))

        (and (nil? status) (= "l" f))
        (do)
          ; (change-status state :l value 1))

        :else
        (throw (ex-info "Garbage on parsing bencode" {:string fragment}))))))

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
  ; (let [state (atom "")]
  (let [state (atom {:buffer "" :status () :acc-str ""})]
    (fn [fragment]
      ; (println "\nAdding fragment" fragment)
      ; (swap! state str fragment)
      (swap! state update :buffer str fragment)
      (decode-sm state))))
      ; (let [[rest parsed] (decode-fragment @state [])]
      ;   (reset! state rest)
      ;   parsed))))


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
