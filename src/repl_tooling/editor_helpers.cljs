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

(defrecord Browseable [object more-fn attributes])
(defrecord IncompleteObj [more-fn])

(defrecord Error [type message add-data trace])
(defn- parse-error [{:keys [via trace cause] :as error}]
  (let [info (or (first via) error)
        {:keys [type message]} info]
    (->Error type (or cause message) (dissoc info :type :message :at :trace) trace)))

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

(defn read-result [res]
  (try
    (edn/read-string {:readers {'unrepl/string #(IncompleteStr. %)
                                'js #(WithTag. % "js")
                                'unrepl/bad-keyword (fn [[ns name]] (keyword ns name))
                                'unrepl/bad-symbol (fn [[ns name]] (symbol ns name))
                                'unrepl/bigint (fn [n] (LiteralRender. (str n "N")))
                                'unrepl/bigdec (fn [n] (LiteralRender. (str n "M")))
                                'unrepl.java/class (fn [k] (WithTag. k "class"))
                                'unrepl/browsable (fn [[a b]]
                                                    (->browseable a b))
                                'repl-tooling/literal-render #(LiteralRender. %)
                                'clojure/var #(->> % (str "#'") symbol)
                                'error parse-error
                                'unrepl/object as-obj}
                      :default #(WithTag. %2 %1)}
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
  (str/replace text #"(\".(\\\"|[^\"])*\"|;.*)"
               (fn [[match]]
                 (cond-> match
                         (str/starts-with? match ";")
                         (str/replace #"." " ")))))

(def delim #{"(" "[" "{"})
(def closes {"(" ")"
             "[" "]"
             "{" "}"})

(defn text-in-range [text [[row1 col1] [row2 col2]]]
  (let [lines (str/split-lines text)
        rows-offset (- row2 row1)]
    (-> lines
        (subvec row1 (inc row2))
        (update 0 #(str/join "" (drop col1 %)))
        (update rows-offset #(str/join "" (take (inc (if (zero? rows-offset)
                                                       (- col2 col1)
                                                       col2))
                                                %)))
        (->> (str/join "\n")))))

(defn- simple-read [str]
  (edn/read-string {:default (fn [_ res] res)} str))

(defn position
  "Returns the zero-indexed position in a code string given line and column."
  [code-str row col]
  (let [row (dec row)
        col (dec col)])
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

(def ^:private openers #{\[ \( \{})
(defn next-open-start [code-str start-position]
  (if (contains? openers (nth code-str start-position))
    start-position
    (let [code-str-prefix (subs code-str start-position)]
      (->> openers
           (map #(str/index-of code-str-prefix %))
           (remove nil?)
           (first)
           (+ start-position)))))

(defn read-next
  "Reads the next expression from some code. Uses an `indexing-pushback-reader`
  to determine how much was read, and return that substring of the original
  `code-str`, rather than what was actually read by the reader."
  ([code-str start-row start-col]
   (binding [reader/resolve-symbol identity
             reader/*suppress-read* true]
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
                       (reader-types/get-column-number rdr))))))
  ([code-str start-position]
   (binding [reader/resolve-symbol identity
             reader/*suppress-read* true]
     (let [code-str (subs code-str start-position)
           rdr (reader-types/indexing-push-back-reader code-str)]
       ;; Read a form, but discard it, as we want the original string.
       (reader/read rdr)
       (subs code-str
             0
             ;; Even though this returns the position *after* the read, this works
             ;; because subs is end point exclusive.
             (position code-str
                       (reader-types/get-line-number rdr)
                       (reader-types/get-column-number rdr)))))))

(defn- count-nls [text row]
  (->> text (re-seq #"\n") count (+ row)))

(defn code-frag [code curr-pos]
  (try
    (read-next code curr-pos)
    (catch ExceptionInfo e
      (if (-> e .-data :ex-kind (= :eof))
        nil
        (throw e)))))

(defn top-levels
  "Gets all top-level ranges for the current code"
  [code]
  (loop [row 0 col 0 old-pos 0 sofar []]
    (let [curr-pos (try (next-open-start code old-pos) (catch :default e nil))
          code-frag (delay (code-frag code curr-pos))]
      (if (and curr-pos @code-frag)
        (let [code-frag @code-frag
              before-code (subs code curr-pos old-pos)
              new-row (count-nls before-code row)
              new-col (cond-> (-> before-code (str/split-lines) last count)
                              (= new-row row) (+ col))
              end-row (count-nls code-frag new-row)
              end-col (cond-> (-> code-frag (str/split-lines) last count)
                              (= end-row new-row) (+ new-col))]
          (recur
            end-row
            end-col
            (+ curr-pos (count code-frag))
            (conj sofar [[[new-row new-col] [end-row (dec end-col)]] code-frag])))
        sofar))))

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

(defn block-for
  "Gets the current block from the code (a string) to the current row and col (0-based)"
  [code [row col]]
  (let [pos (search-start code (inc row) (inc col))
        block (read-next code pos)
        block-lines (str/split-lines block)
        reds (->> code
                  str/split-lines
                  (reductions #(-> %2 count (+ %1)) 0)
                  (take-while #(<= % pos)))
        row (-> reds count dec)
        col (- pos (last reds))
        last-row (-> block-lines count dec (+ row))
        last-col (if (= row last-row)
                   (-> block count (+ col) dec)
                   (-> block-lines last count dec))]
    [[[row col] [last-row last-col]] block]))


(defn top-block-for
  "Gets the top-level from the code (a string) to the current row and col (0-based)"
  [code [row col]]
  (let [tops (top-levels code)
        in-range? (fn [[[[b-row b-col] [e-row e-col]]]]
                    (or (and (<= b-row row) (< row e-row))
                        (and (<= b-row row e-row)
                             (<= b-col col e-col))))]
    (->> tops (filter in-range?) first)))
