(ns ___repl-tooling.__generic_printer_blob)

(defmulti serialize #(-> % type str))

#?(:clje
   (extend-protocol clojerl.IHash
     clojerl.reader.TaggedLiteral
     (hash [this]
       (+ (clojerl.IHash/hash (get this :tag))
          (clojerl.IHash/hash (get this :form))))))

(defmethod serialize "erlang.Tuple" [res]
  (tagged-literal 'erl (serialize (vec res))))

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

    (string? res) res
    :else (tagged-literal 'repl-tooling/literal-render (pr-str res))))

(defn nrepl-pprint [value writer opts]
  (.write writer (pr-str (serialize value))))

(ns user)
:DONE-BLOB
