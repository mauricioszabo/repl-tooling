(ns repl-tooling.features.autocomplete.simple
  (:require [clojure.string :as str]
            [clojure.core.async :as async :include-macros true]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]))

(def special-forms
  (mapv str
       '(case* catch def defrecord* deftype* do finally fn* if js* let*
          letfn* loop* new ns quote recur set! throw try)))

(def ^:private valid-prefix #"/?([a-zA-Z0-9\-.$!?\/><*=\?_]+)")

(defn- normalize-results [{:keys [result]}]
  (vec (some->> result
                helpers/read-result
                (map (fn [c] {:type :function :candidate c})))))

(defn for-clj [repl ns-name txt-prefix]
  (let [chan (async/promise-chan)
        prefix (->> txt-prefix (re-seq valid-prefix) last last str)
        have-prefix? (re-find #"/" prefix)
        ns-part (if have-prefix?
                  (str/replace prefix #"/.*" "")
                  ns-name)]
    (if (not-empty prefix)
      (eval/evaluate repl
                     (str "(clojure.core/let [collect #(clojure.core/map "
                                                        "(clojure.core/comp str first) "
                                                        "(%1 %2)) "
                                              "refers (collect clojure.core/ns-map *ns*)"
                                              "from-ns (->> (clojure.core/ns-aliases *ns*) "
                                                        "(clojure.core/mapcat (fn [[k v]] "
                                                          "(clojure.core/map #(str k \"/\" %) "
                                                          "(collect clojure.core/ns-publics v)))))] "
                           "(clojure.core/->> refers "
                                             "(concat from-ns) "
                                             "(clojure.core/filter #(re-find #\""
                                                                    txt-prefix "\" %)) "
                                             "(clojure.core/sort)"
                           "))")
                     {:namespace ns-name :ignore true}
                     #(async/put! chan (normalize-results %)))
      (async/put! chan []))
    chan))

(defn for-cljs [repl ns-name prefix]
  (let [chan (async/promise-chan)
        prefix (->> prefix (re-seq valid-prefix) last last str)
        have-prefix? (re-find #"/" prefix)
        ns-part (if have-prefix?
                  (str/replace prefix #"/.*" "")
                  ns-name)
        ex-name (str ns-part "/a")]
    (if (not-empty prefix)
      (eval/evaluate repl
                     (str "(cljs.core/let [ns-name (cljs.core/str `" ex-name ") "
                          "                splitted (js->clj (.split ns-name #\"[\\./]\"))\n"
                          "                ns-part (map cljs.core/munge (clojure.core/butlast splitted))"
                          "                from-ns (js->clj (.keys js/Object (apply aget (.-global js/goog) ns-part)))"
                          (when have-prefix?
                            (str " from-ns (map #(str \"" ns-part "/\" %) from-ns)"))
                          "      from-core "
                          (if have-prefix?
                            "nil"
                            "(js->clj (.keys js/Object (aget js/goog \"global\" \"cljs\" \"core\")))")
                          "      both (concat from-ns from-core " special-forms ")]"
                          "(->> both"
                          "     (clojure.core/map cljs.core/demunge)"
                          "     (clojure.core/filter #(clojure.core/re-find #\"" prefix "\" %))"
                          "     (clojure.core/sort)"
                          "     (clojure.core/take 50)"
                          "))")
                     {:namespace ns-name :ignore true}
                     #(async/put! chan (normalize-results %)))
      (async/put! chan []))
    chan))
