(ns repl-tooling.editor-integration.interactive
  (:require [clojure.walk :as walk]
            [repl-tooling.eval :as eval]
            [reagent.core :as r]
            [repl-tooling.editor-helpers :as helpers]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [cljs.tools.reader :as reader]
            [repl-tooling.editor-integration.renderer.protocols :as proto]))

(defn edn? [obj]
  (or (number? obj)
      (string? obj)
      (coll? obj)
      (boolean? obj)
      (nil? obj)
      (regexp? obj)
      (symbol? obj)
      (keyword? obj)))

(defn norm-evt [obj depth]
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

(defn- norm-fn [element state fns repl]
  (let [fn-name (-> element str (str/replace-first #"\?" "") keyword)
        fun (fn-name fns)]
    (fn [e] fun
      (.preventDefault e)
      (.stopPropagation e)
      (.. (eval/eval repl (str "(" fun " " (pr-str (norm-evt (.-target e) 1)) " " (pr-str @state) ")") {:ignore true})
          (then #(reset! state (:result %)))))))

(defn- norm-html [html state fns repl]
  (walk/postwalk (fn [e]
                   (cond
                     (= '?state e) @state

                     (and (symbol? e) (-> e str (str/starts-with? "?")))
                     (norm-fn e state fns repl)

                     :else e))
                 html))

(defn- render-interactive [{:keys [state html fns]} repl]
  (def state state)
  (def html html)
  (def fns fns)
  (let [state (r/atom state)
        html (fn [state] (norm-html html state fns repl))]
    [html state]))

(defrecord Interactive [edn repl editor-state]
  proto/Renderable
  (as-html [_ ratom _]
    (render-interactive edn repl)))
