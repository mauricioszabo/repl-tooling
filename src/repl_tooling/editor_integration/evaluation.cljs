(ns repl-tooling.editor-integration.evaluation
  (:require [clojure.string :as str]
            [repl-tooling.editor-helpers :as helpers]))

(defn need-cljs? [config filename]
  (or
   (-> config :eval-mode (= :cljs))
   (and (-> config :eval-mode (= :discover))
        (str/ends-with? (str filename) ".cljs"))
   (and (-> config :eval-mode (= :prefer-cljs))
        (or (str/ends-with? (str filename) ".cljs")
            (str/ends-with? (str filename) ".cljc")
            (str/ends-with? (str filename) ".cljx")))))

#_
(defn evaluate-aux [editor-data-map conn-opts config opts callback]
  (let [[start] (:range editor-data-map)
        filename (:filename editor-data-map)
        [_ ns-name] (helpers/ns-range-for (:contents editor-data-map) start)
        [row col] start]
    (if (need-cljs? config filename)
      (throw (ex-info "NOT YET IMPLEMENTED" {}))
      (some-> conn-opts :clj/aux
              (eval/evaluate code
                             {:namespace ns-name :row row :col col :filename filename
                              :pass opts}
                             #(-> % helpers/parse-result callback))))))
