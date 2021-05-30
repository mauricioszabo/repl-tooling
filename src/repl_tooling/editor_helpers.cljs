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
            [duck-repled.editor-helpers :as helpers]
            [schema.core :as s]
            ["fs" :as fs]
            ["path" :as path]))

(deftype LiteralRender [string]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer string))
  Object
  (toString [_] (str string)))

(deftype Interactive [edn]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer edn)))

(defprotocol IIncompleteStr
  (only-str [_])
  (concat-with [_ other]))

(deftype IncompleteStr [string]
  Object
  (toString [this] (str (only-str this) "..."))

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

(defrecord Browseable [object more-fn attributes]
  Object
  (toString [_] (str object)))

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

(defn error-result
  ([type message trace] (error-result type message trace nil))
  ([type message trace add-data]
   (let [error (->> add-data
                    (merge {:type type :message message :trace trace})
                    (tagged-literal 'error)
                    pr-str)]
     {:error error :as-text error})))

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
                                'unrepl/ns (fn [n] (LiteralRender. (str n)))
                                'unrepl/bigdec (fn [n] (LiteralRender. (str n "M")))
                                'unrepl.java/class (fn [k] (WithTag. k "class"))
                                'unrepl/browsable (fn [[a b]]
                                                    (->browseable a b))
                                'unrepl/pattern re-pattern
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

(def text-in-range helpers/text-in-range)
(def top-levels helpers/top-blocks)
(def current-var helpers/current-var)
(def block-for helpers/block-for)

(defn top-block-for [text position]
  (let [tops (helpers/top-blocks text)]
    (helpers/top-block-for tops position)))

(defn ns-range-for [text position]
  (let [tops (helpers/top-blocks text)]
    (helpers/ns-range-for tops position)))

(def ^:dynamic *out-on-aux* false)

(s/defn get-possible-port :- (s/maybe s/Int)
  [project-paths :- [s/Str], detect-nrepl? :- s/Bool, typed-port :- (s/maybe s/Int)]
  (if typed-port
    typed-port
    (let [files (cond-> [".socket-repl-port"
                         (path/join ".shadow-cljs" "socket-repl.port")]
                        detect-nrepl? (conj ".nrepl-port"))
          paths (for [file files
                      path project-paths
                      :let [full-path (path/join path file)]
                      :when (fs/existsSync full-path)]
                  (-> full-path fs/readFileSync str js/parseInt))]
      (first paths))))
