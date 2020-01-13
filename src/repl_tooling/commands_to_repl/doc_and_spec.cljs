(ns repl-tooling.commands-to-repl.doc-and-spec
  (:require [clojure.spec.alpha]))

(defn spec2interactive [sym]
  (clojure.core/let [spec-edn (clojure.core/or (clojure.core/some-> sym
                                                                    clojure.spec.alpha/get-spec
                                                                    clojure.spec.alpha/describe)
                                               sym)
                     = clojure.core/=
                     first clojure.core/first
                     s clojure.core/str
                     rest clojure.core/rest
                     partition clojure.core/partition
                     pr-str clojure.core/pr-str
                     map clojure.core/map
                     vector clojure.core/vector
                     name clojure.core/name
                     keyword clojure.core/keyword
                     on-click (clojure.core/fn [symbol]
                                [:eval (s "[:replace (spec2interactive '" symbol " )]")])
                     as-map (clojure.core/fn [edn]
                              (clojure.core/->> edn rest (partition 2 2)
                                 (clojure.core/mapcat
                                  (clojure.core/fn [[k v]]
                                    (case k
                                      :req-un (map (clojure.core/fn [%]
                                                      (vector :div {:key %}
                                                             [:span (s ":" (name %) " ")]
                                                             [:a {:href "#"
                                                                  :on-click (on-click %)}
                                                              (pr-str %)])) v))))))]
    [:html
     (clojure.core/cond
       (clojure.core/not (clojure.core/list? spec-edn)) [:div.other (s spec-edn)]

       (clojure.core/-> spec-edn first (= 'keys))
       [:div.coll.map.row
        [:div.delim.open] "{"
        [:div.children (as-map spec-edn)]
        [:div.delim.close "}"]])]))
