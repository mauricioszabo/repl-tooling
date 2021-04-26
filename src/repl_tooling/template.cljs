(ns repl-tooling.template
  (:require [clojure.walk :as walk]
            [paprika.collection :as coll]))

(defn template [code replaces]
  (let [code (->> code
                  (walk/postwalk (fn [sym]
                                   (if (and (symbol? sym)
                                            (-> sym namespace (= "cljs.core")))
                                     (symbol "clojure.core" (name sym))
                                     sym))))
        symbols (->> code flatten
                     (map (fn [sym] [(-> sym name keyword)
                                     sym]))
                     (into {}))
        replaces (coll/map-keys symbols replaces)]
    (pr-str (walk/postwalk-replace replaces code))))
