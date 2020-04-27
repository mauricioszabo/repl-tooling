(ns repl-tooling.editor-integration.interactive
  (:require [clojure.walk :as walk]
            [repl-tooling.eval :as eval]
            [reagent.core :as r]
            [repl-tooling.editor-helpers :as helpers]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [cljs.tools.reader :as reader]
            [repl-tooling.editor-integration.renderer.protocols :as proto]
            [sci.core :as sci]))

(defn- edn? [obj]
  (or (number? obj)
      (string? obj)
      (coll? obj)
      (boolean? obj)
      (nil? obj)
      (regexp? obj)
      (symbol? obj)
      (keyword? obj)))

(defn- norm-evt [obj depth]
  (cond
    (>= depth 3) obj

    (not (edn? obj))
    (->> obj
         (.keys js/Object)
         js->clj
         (map (fn [k] [(keyword k) (norm-evt (aget obj k) (inc depth))]))
         (filter (fn [[_ v]] (edn? v)))
         (into {}))

    :else obj))

(defn- prepare-fn [fun state repl]
  (fn [e] fun
    (.preventDefault e)
    (.stopPropagation e)
    (.. (eval/eval repl
                   (str "(" fun " "
                        (pr-str (norm-evt (.-target e) 1))
                        " '" (pr-str @state) ")")
                   {:ignore true})
        (then #(reset! state (:result %))))))

(defn- bindings-for [state fns repl]
  (->> fns
       (map (fn [[f-name f-body]] [(->> f-name name (str "?") symbol)
                                   (prepare-fn f-body state repl)]))
       (into {'?state @state})))

(def ^:private walk-ns {'postwalk walk/postwalk
                        'prewalk walk/prewalk
                        'keywordize-keys walk/keywordize-keys
                        'walk walk/walk
                        'postwalk-replace walk/postwalk-replace
                        'prewalk-replace walk/prewalk-replace
                        'stringify-keys walk/stringify-keys})

(defn- render-interactive [{:keys [state html fns] :as edn} repl]
  (def state state)
  (def html html)
  (def fns fns)
  (let [state (r/atom state)
        html (fn [state]
               (try
                (sci/eval-string (pr-str html) {:bindings (bindings-for state fns repl)
                                                :preset {:termination-safe true}
                                                :namespaces {'walk walk-ns}})
                (catch :default e
                  [:div.error "Can't render this code - " (pr-str e)])))]
    [html state]))

(defrecord Interactive [edn repl editor-state]
  proto/Renderable
  (as-html [_ ratom _]
    (render-interactive edn repl)))
