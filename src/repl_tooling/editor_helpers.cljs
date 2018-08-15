(ns repl-tooling.editor-helpers
  (:require [clojure.string :as str]))

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
  (let [text (str/split-lines text)]
    (loop [acc {:forms []}
           {:keys [current close depth start] :as state} nil
           [row col] [0 0]]

      (let [char (get-in text [row col])
            next (next-pos row col text)]
        (cond
          (nil? row)
          acc

          (and (nil? current) (delim char))
          (recur acc {:current char :close (closes char) :depth 0 :start [row col]}
            next)

          (delim char)
          (recur acc (update state :depth inc) next)

          (and (zero? depth) (= close char))
          (recur (update acc :forms conj [start [row col]]) nil next)

          (= close char)
          (recur acc (update state :depth dec) next)

          :else
          (recur acc state next))))))
