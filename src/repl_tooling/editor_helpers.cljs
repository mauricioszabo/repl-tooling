(ns repl-tooling.editor-helpers
  (:require [clojure.string :as str]
            [cljs.reader :as edn]
            [clojure.tools.reader.reader-types :as reader-types]
            [clojure.tools.reader :as reader]))

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

(edn/read-string "((99 99) \"0x19f09740\" \"[99, 99]\" {:bean {{:repl-tooling/... nil} {:repl-tooling/... (unrepl.repl$LCGtW9Cgr88YHErE3u8GL_79IP4/fetch :G__202400)}}})")

(defrecord Browseable [object more-fn attributes])
(defrecord IncompleteObj [more-fn])

(defn- ->browseable [object additional-data]
  (cond
    (and (instance? WithTag object) (= "#class " (tag object)))
    (let [[f s] (obj object)] (->Browseable f (:repl-tooling/... s) nil))

    (and (map? object) (-> object keys (= [:repl-tooling/...])))
    (->IncompleteObj (:repl-tooling/... object))

    :else
    (->Browseable object (:repl-tooling/... additional-data) nil)))

(declare read-result)
(defn as-obj [data]
  (let [params (last data)
        [browseable pr-str-obj obj-id repr] data]
    (if pr-str-obj
      (->browseable pr-str-obj (get (:bean params) {:repl-tooling/... nil}))
      (->browseable (str (:object browseable) "@" obj-id) (get (:bean params) {:repl-tooling/... nil})))))

(defn- default-tag [tag data]
  (case (str tag)
    "clojure/var" (->> data (str "#'") symbol)
    "unrepl/object" (as-obj data)
    (WithTag. data tag)))

(defn read-result [res]
  (try
    (edn/read-string {:readers {'unrepl/string #(IncompleteStr. %)
                                'unrepl/bad-keyword (fn [[ns name]] (keyword ns name))
                                'unrepl/bad-symbol (fn [[ns name]] (symbol ns name))
                                'unrepl/bigint (fn [n] (LiteralRender. (str n "N")))
                                'unrepl/bigdec (fn [n] (LiteralRender. (str n "M")))
                                'unrepl.java/class (fn [k] (WithTag. k "class"))
                                'unrepl/browsable (fn [[a b]]
                                                    (->browseable a b))
                                'repl-tooling/literal-render #(LiteralRender. %)
                                'clojure/var #(->> % (str "#'") symbol)
                                'unrepl/object as-obj}
                      :default default-tag}
                     res)
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

(defn text-in-range [text [[row1 col1] [row2 col2]]]
  (let [lines (str/split-lines text)]
    (-> lines
        (subvec row1 (inc row2))
        (update 0 #(str/join "" (drop col1 %)))
        (update (- row2 row1) #(str/join "" (take (inc col2) %)))
        (->> (str/join "\n")))))

(defn- simple-read [str]
  (edn/read-string {:default (fn [_ res] res)} str))

; (defn current-top-block [text row col]
;   (let [levels (top-levels text)]))

(defn position
  "Returns the zero-indexed position in a code string given line and column."
  [code-str row col]
  (->> code-str
       str/split-lines
       (take (dec row))
       (str/join)
       count
       (+ col (dec row)) ;; `(dec row)` to include for newlines
       dec
       (max 0)))

(defn search-start
  "Find the place to start reading from. Search backwards from the starting
  point, looking for a '[', '{', or '('. If none can be found, search from
  the beginning of `code-str`."
  ([code-str start-row start-col]
   (search-start code-str (position code-str start-row start-col)))
  ([code-str start-position]
   (let [openers #{\[ \( \{}]
     (if (contains? openers (nth code-str start-position))
       start-position
       (let [code-str-prefix (subs code-str 0 start-position)]
         (->> openers
              (map #(str/last-index-of code-str-prefix %))
              (remove nil?)
              (apply max 0)))))))

(defn read-next
  "Reads the next expression from some code. Uses an `indexing-pushback-reader`
  to determine how much was read, and return that substring of the original
  `code-str`, rather than what was actually read by the reader."
  ([code-str start-row start-col]
   (let [code-str (subs code-str (search-start code-str start-row start-col))
         rdr (reader-types/indexing-push-back-reader code-str)]
     ;; Read a form, but discard it, as we want the original string.
     (reader/read rdr)
     (subs code-str
           0
           ;; Even though this returns the position *after* the read, this works
           ;; because subs is end point exclusive.
           (position code-str
                     (reader-types/get-line-number rdr)
                     (reader-types/get-column-number rdr)))))
  ([code-str start-position]
   (let [code-str (subs code-str (search-start code-str start-position))
         rdr (reader-types/indexing-push-back-reader code-str)]
     ;; Read a form, but discard it, as we want the original string.
     (reader/read rdr)
     (subs code-str
           0
           ;; Even though this returns the position *after* the read, this works
           ;; because subs is end point exclusive.
           (position code-str
                     (reader-types/get-line-number rdr)
                     (reader-types/get-column-number rdr))))))

(defn top-levels
  "Gets all top-level ranges for the current code"
  [text]
  (let [size (count text)]
    (loop [curr-pos 0
           row 0
           col 0
           tops []]

      (cond
        (>= curr-pos size) tops
        (re-find #"\n" (nth text curr-pos)) (recur (inc curr-pos) (inc row) 0 tops)
        (re-find #"\s" (nth text curr-pos)) (recur (inc curr-pos) row (inc col) tops)
        :else (let [nxt (read-next text curr-pos)
                    last-row (->> nxt (re-seq #"\n") count (+ row))
                    last-col (-> nxt str/split-lines last count (+ col))]
                (recur
                  (+ (count nxt) curr-pos)
                  last-row
                  last-col
                  (conj tops [[[row col]
                               [last-row (dec last-col)]]
                              nxt])))))))

(defn ns-range-for
  "Gets the current NS range (and ns name) for the current code, considering
that the cursor is in row and col (0-based)"
  [code [row col]]
  (let [levels (top-levels code)
        before-selection? (fn [[[[_ _] [erow ecol]] _]]
                            (or (and (= erow row) (<= col ecol))
                                (< erow row)))
        is-ns? #(and (list? %) (some-> % first (= 'ns)))
        read #(try (simple-read %) (catch :default _ nil))]

    (->> levels
         (take-while before-selection?)
         reverse
         (map #(update % 1 read))
         (filter #(-> % peek is-ns?))
         (map #(update % 1 second))
         first)))

(defn top-block-for
  "Gets the top-level from the code (a string) to the current row and col (0-based)"
  [code [row col]]
  (let [tops (top-levels code)
        in-range? (fn [[[[b-row b-col] [e-row e-col]]]]
                    (or (and (<= b-row row) (< row e-row))
                        (and (<= b-row row e-row)
                             (<= b-col col e-col))))]
    (->> tops (filter in-range?) first)))
