(ns repl-tooling.editor-helpers
  (:require [clojure.string :as str]
            [cljs.reader :as edn]
            [cljs.tools.reader :as reader]
            [rewrite-clj.zip.move :as move]
            [rewrite-clj.zip :as zip]
            [rewrite-clj.zip.base :as zip-base]
            [rewrite-clj.node :as node]
            [rewrite-clj.reader :as clj-reader]
            [rewrite-clj.parser :as parser]
            [repl-tooling.editor-integration.schemas :as schemas]
            [schema.core :as s]))

(deftype LiteralRender [string]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer string)))

(deftype Interactive [edn]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer edn)))

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
  (let [info (or (some-> via reverse first) error)
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

(defrecord Patchable [id value])

(defn as-obj [data]
  (let [params (last data)
        [browseable pr-str-obj obj-id] data]
    (if pr-str-obj
      (->browseable pr-str-obj (get (:bean params) {:repl-tooling/... nil}))
      (->browseable (str (:object browseable) "@" obj-id) (get (:bean params) {:repl-tooling/... nil})))))

(defn- read-result [res]
  (try
    (edn/read-string {:readers {'unrepl/string #(IncompleteStr. %)
                                'js #(WithTag. % "js")
                                'unrepl/bad-keyword (fn [[ns name]] (keyword ns name))
                                'unrepl/bad-symbol (fn [[ns name]] (symbol ns name))
                                'unrepl/ratio (fn [[n d]] (LiteralRender. (str n "/" d)))
                                'unrepl/bigint (fn [n] (LiteralRender. (str n "N")))
                                'unrepl/bigdec (fn [n] (LiteralRender. (str n "M")))
                                'unrepl.java/class (fn [k] (WithTag. k "class"))
                                'unrepl/browsable (fn [[a b]]
                                                    (->browseable a b))
                                'repl-tooling/literal-render #(LiteralRender. %)
                                'repl-tooling/interactive #(Interactive. %)
                                'repl-tooling/patchable #(->Patchable (first %) (second %))
                                'clojure/var #(->> % (str "#'") symbol)
                                'error parse-error
                                'unrepl/object as-obj}
                      :default #(WithTag. %2 %1)}
                     res)
    (catch :default _
      (symbol res))))

(s/defn parse-result :- schemas/ReplResult [result :- s/Any]
  (assoc (if (contains? result :result)
           (update result :result #(if (:parsed? result)
                                     %
                                     (cond-> (read-result %)
                                             (:literal result) LiteralRender.
                                             (:interactive result) Interactive.)))
           (update result :error #(cond-> % (not (:parsed? result)) read-result)))
         :parsed? true))

(defn text-in-range [text [[row1 col1] [row2 col2]]]
  (let [lines (str/split-lines text)
        rows-offset (- (min row2 (count lines)) row1)]
    (-> lines
        (subvec row1 (min (count lines) (inc row2)))
        (update 0 #(str/join "" (drop col1 %)))
        (update rows-offset #(str/join "" (take (inc (if (zero? rows-offset)
                                                       (- col2 col1)
                                                       col2))
                                                %)))
        (->> (str/join "\n")))))

(defn- simple-read [str]
  (reader/read-string {:default (fn [_ res] res)} str))

(defn- parse-reader [reader]
  (try
    (let [parsed (parser/parse reader)]
      (when parsed
        (cond
          (node/whitespace-or-comment? parsed) :whitespace

          (instance? rewrite-clj.node.uneval/UnevalNode parsed)
          (->> parsed :children (remove node/whitespace-or-comment?) first)

          :else parsed)))
    (catch :default _
      (clj-reader/read-char reader)
      :whitespace)))

(defn top-levels
  "Gets all top-level ranges for the current code"
  [code]
  (let [reader (clj-reader/indexing-push-back-reader code)]
    (loop [sofar []]
      (let [parsed (parse-reader reader)]
        (case parsed
          :whitespace (recur sofar)
          nil sofar
          (let [as-str (node/string parsed)
                {:keys [row col end-row end-col]} (meta parsed)]
            (recur (conj sofar [[[(dec row) (dec col)]
                                 [(dec end-row) (- end-col 2)]]
                                as-str]))))))))

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

(defn in-range? [{:keys [row col end-row end-col]} {r :row c :col}]
  (and (>= r row)
       (<= r end-row)
       (if (= r row) (>= c col) true)
       (if (= r end-row) (<= c end-col) true)))

(defn find-inners-by-pos
  "Find last node (if more than one node) that is in range of pos and
  satisfying the given predicate depth first from initial zipper
  location."
  [zloc pos]
  (->> zloc
       (iterate zip/next)
       (take-while identity)
       (take-while (complement move/end?))
       (filter #(in-range? (-> % zip/node meta) pos))))

(defn- reader-tag? [node]
  (when node
    (or (instance? rewrite-clj.node.reader-macro.ReaderMacroNode node)
        (instance? rewrite-clj.node.fn/FnNode node)
        (instance? rewrite-clj.node.quote.QuoteNode node)
        (instance? rewrite-clj.node.reader-macro.DerefNode node))))

(defn- filter-forms [nodes]
  (when nodes
    (let [valid-tag? (comp #{:vector :list :map :set :quote} :tag)]
      (->> nodes
           (map zip/node)
           (partition-all 2 1)
           (map (fn [[fst snd]]
                  (cond
                    (reader-tag? fst) fst
                    (-> fst :tag (= :list) (and snd (reader-tag? snd))) snd
                    (valid-tag? fst) fst)))
           (filter identity)
           first))))

(defn- zip-from-code [code]
  (let [reader (clj-reader/indexing-push-back-reader code)
        nodes (->> (repeatedly #(try
                                  (parser/parse reader)
                                  (catch :default _
                                    (clj-reader/read-char reader)
                                    (node/whitespace-node " "))))
                   (take-while identity)
                   (doall))
        all-nodes (with-meta
                    (node/forms-node nodes)
                    (meta (first nodes)))]
    (-> all-nodes zip-base/edn)))

(defn- current-var* [zipped row col]
  (let [node (-> zipped
                 (zip/find-last-by-pos {:row (inc row) :col (inc col)})
                 zip/node)]
    (when (and node (-> node node/whitespace-or-comment? not))
      (let [{:keys [row col end-row end-col]} (meta node)]
        [[[(dec row) (dec col)] [(dec end-row) (- end-col 2)]]
         (node/string node)]))))

(defn current-var [code [row col]]
  (let [zipped (zip-from-code code)]
    (or (current-var* zipped row col)
        (current-var* zipped row (dec col)))))

(defn block-for
  "Gets the current block from the code (a string) to the current row and col (0-based)"
  [code [row col]]
  (let [node-block (-> code
                       zip-from-code
                       (find-inners-by-pos {:row (inc row) :col (inc col)})
                       reverse
                       filter-forms)
        {:keys [row col end-row end-col]} (some-> node-block meta)]
    (when node-block
      [[[(dec row) (dec col)] [(dec end-row) (- end-col 2)]]
       (node/string node-block)])))

(defn top-block-for
  "Gets the top-level from the code (a string) to the current row and col (0-based)"
  [code [row col]]
  (let [tops (top-levels code)
        in-range? (fn [[[[b-row b-col] [e-row e-col]]]]
                    (or (and (<= b-row row) (< row e-row))
                        (and (<= b-row row e-row)
                             (or (<= b-col col e-col)
                                 (<= b-col (dec col) e-col)))))]
    (->> tops (filter in-range?) first)))
