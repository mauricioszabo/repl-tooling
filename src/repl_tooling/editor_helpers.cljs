(ns repl-tooling.editor-helpers
  (:require [clojure.string :as str]
            [cljs.reader :as reader]))

(deftype LiteralRender [string]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer string)))

(defprotocol IIncompleteStr
  (only-str [_])
  (concat-with [_ other]))

(deftype IncompleteStr [string]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer (pr-str (str (first string) "..."))))

  IIncompleteStr
  (only-str [_] (first string))
  (concat-with [_ other]
    (if (string? other)
      (str (first string) other)
      (IncompleteStr. [(str (first string) (only-str other))
                       {:repl-tooling/... (-> other meta :get-more)}])))

  IMeta
  (-meta [coll] {:get-more (-> string second :repl-tooling/...)}))

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
    (reader/read-string {:readers {'unrepl/string #(IncompleteStr. %)
                                   'repl-tooling/literal-render #(LiteralRender. %)}
                         :default default-tag} res)
    (catch :default _
      (symbol res))))

(defn parse-result [result]
  (if (:result result)
    (update result :result read-result)
    (update result :error read-result)))

(defn strip-comments [text]
  (-> text
      (str/replace #"\".(\\\"|[^\"])*\"" (fn [[a]] (str/replace a #";" " ")))
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

          (= current char)
          (recur forms (update state :depth inc) next)

          (and (zero? depth) (= close char))
          (recur (conj forms [start [row col]]) nil next)

          (= close char)
          (recur forms (update state :depth dec) next)

          :else
          (recur forms  state next))))))

(def text (.. js/atom -workspace getActiveTextEditor getText))
(def levels (top-levels text))
(strip-comments text)

(defn text-in-range [text [[row1 col1] [row2 col2]]]
  (let [lines (str/split-lines text)]
    (-> lines
        (subvec row1 (inc row2))
        (update 0 #(str/join "" (drop col1 %)))
        (update (- row2 row1) #(str/join "" (take col2 %)))
        (->> (str/join "\n")))))

(defn current-top-block [text row col]
  (let [levels (top-levels text)]))
