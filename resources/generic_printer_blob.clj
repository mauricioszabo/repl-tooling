(ns ___repl-tooling.__generic_printer_blob
  (:require [clojure.string :as str]))

(defmulti serialize #(-> % type pr-str))

#?(:cljs
   (defn norm-js-obj [js-obj]
     (tagged-literal
      'unrepl/browsable
      [(if (= js/Function (type js-obj))
         (let [splitted (-> js-obj .-name cljs.core/demunge
                            (clojure.string/split (re-pattern "/")))]
           (tagged-literal 'unrepl/bad-symbol
                           [(->> splitted
                                 butlast
                                 (clojure.string/join ".")
                                 not-empty)
                            (str (last splitted) " (function)")]))
         (if (try (cljs.reader/read-string {:default tagged-literal}
                                           (pr-str js-obj))
               (catch :default _ nil))
           js-obj
           (tagged-literal 'unrepl/bad-symbol [nil (pr-str js-obj)])))
       {:repl-tooling/... `(quote
                             ~(->> js-obj
                                   js/Object.getPrototypeOf
                                   js/Object.getOwnPropertyNames
                                   (concat (js/Object.getOwnPropertyNames js-obj))
                                   distinct
                                   sort
                                   (map #(symbol (str "." %)))))}])))

#?(:clje
   (extend-protocol clojerl.IHash
     clojerl.reader.TaggedLiteral
     (hash [this]
       (+ (clojerl.IHash/hash (get this :tag))
          (clojerl.IHash/hash (get this :form))))))

(defmethod serialize "#object[cljs$core$ExceptionInfo]" [res]
  (tagged-literal 'error
    {:type "cljs.core.ExceptionInfo"
     :data (.-data res)
     :message (.-message res)
     :trace (->> res .-stack clojure.string/split-lines)}))

(defmethod serialize "erlang.Tuple" [res]
  (tagged-literal 'erl (serialize (vec res))))

(defmethod serialize "#object[Promise]" [res]
  (let [id (gensym "patch")]
    (.then res
      (fn [res]
        (tap>
         (tagged-literal
          'repl-tooling/patch
          [id
           (pr-str
            (tagged-literal
             'promise
             (serialize res)))]))))
    (tagged-literal
     'repl-tooling/patchable [id (tagged-literal 'promise '<pending>)])))

(defmethod serialize :default [res]
  (cond
     #?(:cljs false :clje false :default (ratio? res))
     (tagged-literal 'repl-tooling/literal-render (pr-str res))

    (record? res)
    res

    (map? res)
    (->> res (map #(mapv serialize %)) (into {}))

    (vector? res)
    (mapv serialize res)

    (coll? res)
    (map serialize res)

    (var? res)
    (tagged-literal 'repl-tooling/literal-render (pr-str res))

    (->> res type str (re-find #"(?i)regex"))
    (tagged-literal 'repl-tooling/literal-render (pr-str res))

    (symbol? res)
    (let [r (pr-str res)]
      (if (re-matches #"[a-zA-Z0-9\-.$!?\/><*=_]+" r)
        res
        (tagged-literal 'unrepl/bad-symbol [nil (pr-str res)])))

    (keyword? res)
    (let [r (pr-str res)]
      (if (re-matches #"[a-zA-Z0-9\-.$!?\/><*=_]+" r)
        res
        (tagged-literal 'unrepl/bad-keyword [(namespace res) (name res)])))

    (->> res type str (re-find #"Big(Decimal|Float)"))
    (str "#unrepl/bigdec " res)

    (->> res type str (re-find #"BigInt"))
    (str "#unrepl/bigint "res)

    (number? res)
    (if (> res 9007199254740990)
      (tagged-literal 'repl-tooling/literal-render (pr-str res))
      res)

    (contains? [true false nil] res) res

    #?(:cljs (instance? js/Error res))
    #?(:cljs (tagged-literal 'error
               {:type (.-name res)
                :message (.-message res)
                :trace (->> res .-stack clojure.string/split-lines)}))

    (string? res) res
    :else #?(:cljs (norm-js-obj res)
             :default (tagged-literal 'repl-tooling/literal-render (pr-str res)))))

(defn nrepl-pprint [value writer opts]
  (.write writer (pr-str (serialize value))))

(ns user)
:DONE-BLOB
