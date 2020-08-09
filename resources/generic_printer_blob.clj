(ns ___repl-tooling.__generic_printer_blob)

(defmulti serialize #(-> % type str))

(defmethod serialize :default [res]
  (cond
     #?(:cljs false :clje false :default (ratio? res))
     (symbol (pr-str (tagged-literal 'repl-tooling/literal-render (pr-str res))))

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

    (symbol? res)
    (let [r (pr-str res)]
      (if (re-find #"\t" r)
        (tagged-literal 'unrepl/bad-symbol [nil (pr-str res)])
        res))

    (keyword? res)
    (let [r (pr-str res)]
      (if (re-find #"\t" r)
        (tagged-literal 'unrepl/bad-keyword [(namespace res) (name res)])
        res))

    (->> res type str (re-find #"Big(Decimal|Float)"))
    (str "#unrepl/bigdec " res)

    (->> res type str (re-find #"BigInt"))
    (str "#unrepl/bigint "res)

    :else res))

(ns user)
:DONE-BLOB
