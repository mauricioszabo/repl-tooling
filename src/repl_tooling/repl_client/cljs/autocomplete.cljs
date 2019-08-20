(ns repl-tooling.repl-client.cljs.autocomplete
  (:require [clojure.string :as str]
            [repl-tooling.eval :as eval]))

(def special-forms
  (mapv str
       '(case* catch def defrecord* deftype* do finally fn* if js* let*
          letfn* loop* new ns quote recur set! throw try)))

(def ^:private valid-prefix #"/?([a-zA-Z0-9\-.$!?\/><*=\?_]+)")
(defn complete [repl ns-name atom-prefix callback]
  (let [prefix (->> atom-prefix (re-seq valid-prefix) last last)
        have-prefix? (re-find #"/" prefix)
        ns-part (if have-prefix?
                  (str/replace prefix #"/.*" "")
                  ns-name)
        ex-name (str ns-part "/a")]
    (eval/evaluate repl
                   (str "(let [ns-name (cljs.core/str `" ex-name ") "
                        "      splitted (js->clj (.split ns-name #\"[\\./]\"))\n"
                        "      ns-part (map cljs.core/munge (clojure.core/butlast splitted))"
                        "      from-ns (js->clj (.keys js/Object (apply aget (.-global js/goog) ns-part)))"
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
                   callback)))
