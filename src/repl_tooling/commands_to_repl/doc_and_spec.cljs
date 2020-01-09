(ns repl-tooling.commands-to-repl.doc-and-spec)

{:foo :bar}

(defn spec2interactive [spec-edn]
  (clojure.core/let [= clojure.core/=
                     first clojure.core/first
                     s clojure.core/str
                     rest clojure.core/rest
                     partition clojure.core/partition
                     pr-str clojure.core/pr-str
                     map clojure.core/map
                     vector clojure.core/vector
                     name clojure.core/name
                     keyword clojure.core/keyword
                     as-map (clojure.core/fn [edn]
                              (clojure.core/->> edn rest (partition 2 2)
                                                (#(doto % prn))
                                 (clojure.core/mapcat
                                  (clojure.core/fn [[k v]]
                                                   (prn :K k :V v)
                                    (case k
                                      :req-un (map #(vector (keyword (name %)) 'required) v))))
                                 (clojure.core/into {})))]
    (clojure.core/cond
      (clojure.core/not (clojure.core/list? spec-edn)) [:div.other (s spec-edn)]

      (clojure.core/-> spec-edn first (= 'keys))
      [:div.coll.map.row
       [:div.delim.open] "{"
       [:div.children
        (pr-str (as-map spec-edn))]
       [:div.delim.close "}"]])))
