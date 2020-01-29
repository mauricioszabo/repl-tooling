(ns repl-tooling.features.autocomplete.simple
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]))

(def special-forms
  (mapv str
       '(case* catch def defrecord* deftype* do finally fn* if js* let*
          letfn* loop* new ns quote recur set! throw try)))

(def ^:private valid-prefix #"/?([a-zA-Z0-9\-.$!?\/><*=\?_]+)")

(defn- normalize-results [result]
  (vec (some->> result
                helpers/parse-result
                :result
                (map (fn [c] {:type :function :candidate c})))))

(def ^:private re-char-escapes
  (->> "\\.*+|?()[]{}$^"
       set
       (map (juxt identity #(str "\\" %)))
       (into {})))

(defn- re-escape [prefix]
  (str/escape (str prefix) re-char-escapes))

(defn for-clj [repl ns-name txt-prefix]
  (let [prefix (->> txt-prefix (re-seq valid-prefix) last last)
        cmd (str "(clojure.core/let [collect #(clojure.core/map "
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
                                                           (re-escape txt-prefix) "\" %)) "
                                    "(clojure.core/sort)"
                                    "vec"
                  "))")]
    (if (not-empty prefix)
      (.. (eval/eval repl cmd {:namespace ns-name :ignore true})
          (then normalize-results)
          (catch (constantly [])))
      (p/promise []))))

(defn for-cljs [repl ns-name prefix]
  (let [prefix (->> prefix (re-seq valid-prefix) last last str)
        have-prefix? (re-find #"/" prefix)
        ns-part (if have-prefix?
                  (str/replace prefix #"/.*" "")
                  ns-name)
        ex-name (str ns-part "/a")
        cmd
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
             "     (clojure.core/filter #(clojure.core/re-find #\"" (re-escape prefix) "\" %))"
             "     (clojure.core/sort)"
             "     (clojure.core/take 50)"
             "))")]
    (if (not-empty prefix)
      (.. (eval/eval repl cmd {:namespace ns-name :ignore true})
          (then normalize-results)
          (catch (constantly [])))
      (p/promise []))))
