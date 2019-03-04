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
    ;TODO: See if this will work
    (-write writer (pr-str obj)))

  Taggable
  (obj [_] obj)
  (tag [_] (str "#" tag " ")))

(declare read-result)
(defn- as-obj [data]
  (let [params (last data)
        parse-obj (fn [[class obj-id repr]]
                    (WithTag. (merge (:bean params)
                                     {:class class
                                      :object-id obj-id
                                      :repr repr})
                              "object"))]

    (if-let [as-str (:pr-str params)]
      (if (or (instance? IncompleteStr as-str)
              (str/starts-with? (str as-str) "#object["))
        (parse-obj data)
        (read-result as-str))
      (parse-obj data))))

(defn- default-tag [tag data]
  (case (str tag)
    "clojure/var" (->> data (str "#'") symbol)
    "unrepl/object" (as-obj data)
    (WithTag. data tag)))

(defn read-result [res]
  (try
    (reader/read-string {:readers {'unrepl/string #(IncompleteStr. %)
                                   'unrepl/bad-keyword (fn [[ns name]] (keyword ns name))
                                   'unrepl/bad-symbol (fn [[ns name]] (symbol ns name))
                                   'unrepl/bigint (fn [n] (LiteralRender. (str n "N")))
                                   'unrepl/bigdec (fn [n] (LiteralRender. (str n "M")))
                                   'unrepl.java/class (fn [k] (WithTag. k "class"))
                                   ; FIXME: solve in the future this object
                                   'unrepl/browsable (fn [[o]] o)
                                   'repl-tooling/literal-render #(LiteralRender. %)}
                         :default default-tag res})
    (catch :default _
      (symbol res))))

(defn parse-result [result]
  (assoc (if (:result result)
           (update result :result #(if (:parsed? result)
                                     %
                                     (cond-> (read-result %)
                                             (:literal result) LiteralRender.)))
           (update result :error #(cond-> % (not (:parsed? result)) read-result)))
         :parsed? true))

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

(defn text-in-range [text [[row1 col1] [row2 col2]]]
  (let [lines (str/split-lines text)]
    (-> lines
        (subvec row1 (inc row2))
        (update 0 #(str/join "" (drop col1 %)))
        (update (- row2 row1) #(str/join "" (take (inc col2) %)))
        (->> (str/join "\n")))))

(defn- simple-read [str]
  (reader/read-string {:default (fn [_ res] res)} str))

(defn ns-range-for [code [[row col]]]
  (let [levels (top-levels code)
        before-selection? (fn [[[_ _] [erow ecol]]]
                            (or (and (= erow row) (<= col ecol))
                                (< erow row)))
        read-str #(simple-read (text-in-range code %))
        is-ns? #(and (list? %) (some-> % first (= 'ns)))]

    (->> levels
         (take-while before-selection?)
         reverse
         (filter #(-> % read-str is-ns?))
         first)))

(defn ns-name-for [code range]
  (some->> (ns-range-for code range) (text-in-range code) simple-read second))

(defn current-top-block [text row col]
  (let [levels (top-levels text)]))
