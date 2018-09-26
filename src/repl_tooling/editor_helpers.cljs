(ns repl-tooling.editor-helpers
  (:require [clojure.string :as str]
            [cljs.reader :as reader]))

; TODO: I don't know if this belongs here or not
(defprotocol Taggable
  (obj [this])
  (tag [this]))

(deftype WithTag [obj tag]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#")
    (-write writer tag)
    (-write writer " ")
    (-write writer obj))

  Taggable
  (obj [_] obj)
  (tag [_] (str "#" tag " ")))

(defn- default-tag [tag data]
  (case (str tag)
    "clojure/var" (->> data (str "#'") symbol)
    ; "unrepl/object" (as-obj data)
    "unrepl.java/class" (WithTag. data "class")
    (WithTag. data tag)))

(defn read-result [res]
  (try
    (reader/read-string {:default default-tag} res)
    (catch :default _
      (symbol res))))

(defn strip-comments [text]
  (-> text
      (str/replace #"\".(\\\"|[^\"])*\"" (fn [[a]] (apply str (take (count a) (repeat " ")))))
      (str/replace #"\\;" "  ")
      (str/replace #";.*" "")))

(def delim #{"(" "[" "{"})
(def closes {"(" ")"
             "[" "]"
             "{" "}"})

(defn- next-pos [row col text]
  (if (>= col (-> text (get row) count dec))
    (if (>= row (dec (count text)))
      nil
      [(inc row) 0])
    [row (inc col)]))

(defn top-levels [text]
  (let [text (-> text strip-comments str/split-lines)]
    (prn text)
    (loop [forms []
           {:keys [current close depth start] :as state} nil
           [row col] [0 0]]

      (let [char (get-in text [row col])
            next (next-pos row col text)]
        (cond
          (nil? row)
          forms

          (and (nil? current) (delim char))
          (recur forms {:current char :close (closes char) :depth 0 :start [row col]}
            next)

          (delim char)
          (recur forms (update state :depth inc) next)

          (and (zero? depth) (= close char))
          (recur (conj forms [start [row col]]) nil next)

          (= close char)
          (recur forms (update state :depth dec) next)

          :else
          (recur forms  state next))))))
