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

(defn- norm-evt [obj]
  (->> obj
       js/Object.getPrototypeOf
       js/Object.getOwnPropertyNames
       (map #(let [norm (-> %
                            (str/replace #"[A-Z]" (fn [r] (str "-" (str/lower-case r))))
                            keyword)]
               [norm (aget obj %)]))
       (filter (comp edn? second))
       (into {})))

(defn- run-evt-fun! [e fun state repl additional-args]
  (.preventDefault e)
  (.stopPropagation e)
  (.. (eval/eval repl
                 (str "(" fun " '"
                      (pr-str (norm-evt (.-target e)))
                      " '" (pr-str @state)
                      " " (->> additional-args (map #(str "'"(pr-str %))) (str/join " "))
                      ")")
                 {:ignore true})
      (then #(reset! state (:result %)))))

(defn- prepare-fn [fun state repl]
  (fn [ & args]
    (if (-> args first edn?)
      (fn [e] (run-evt-fun! e fun state repl args))
      (run-evt-fun! (first args) fun state repl []))))

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
