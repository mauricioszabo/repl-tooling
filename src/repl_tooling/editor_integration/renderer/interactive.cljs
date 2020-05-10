(ns repl-tooling.editor-integration.renderer.interactive
  (:require [clojure.walk :as walk]
            [repl-tooling.eval :as eval]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [repl-tooling.editor-helpers :as helpers]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [cljs.tools.reader :as reader]
            [repl-tooling.editor-integration.renderer.protocols :as proto]
            [sci.core :as sci]
            [repl-tooling.editor-integration.commands :as cmds]))

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

(defn- treat-error [hiccup]
  (let [d (. js/document createElement "div")]
    (rdom/render hiccup d)
    hiccup))

(defn- editor-ns [repl state]
  {'run-callback (partial cmds/run-callback! state)
   'run-feature (fn [cmd & args]
                  (if (= cmd :go-to-var-definition)
                    (cmds/run-feature! state
                                       :go-to-var-definition
                                       (assoc (first args)
                                              :repl repl))
                    (apply cmds/run-feature! state cmd args)))})

(defn- render-interactive [{:keys [state html fns] :as edn} repl editor-state]
  (let [state (r/atom state)
        html (fn [state]
               (try
                 (-> html
                     pr-str
                     (sci/eval-string  {:bindings (bindings-for state fns repl)
                                        :preset {:termination-safe true}
                                        :namespaces {'walk walk-ns
                                                     'editor (editor-ns repl
                                                                        editor-state)}})
                     treat-error)
                (catch :default e
                  [:div.error "Can't render this code - " (pr-str e)])))]
    [html state]))

(defrecord Interactive [edn repl editor-state]
  proto/Renderable
  (as-html [_ ratom _]
    (render-interactive edn repl editor-state)))