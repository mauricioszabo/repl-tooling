(ns repl-tooling.editor-integration.renderer.interactive
  (:require [reagent.core :as r]
            [promesa.core :as p]
            [reagent.dom :as rdom]
            [paprika.collection :as coll]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.renderer.protocols :as proto]
            [repl-tooling.ui.pinkie :as pinkie]
            [repl-tooling.editor-integration.interpreter :as int]))

(defn- edn? [obj]
  (or (number? obj)
      (string? obj)
      (coll? obj)
      (boolean? obj)
      (nil? obj)
      (regexp? obj)
      (symbol? obj)
      (keyword? obj)
      (tagged-literal? obj)))

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

(defn- run-evt-fun! [eql e fun state repl additional-args]
  (.preventDefault e)
  (.stopPropagation e)
  (let [norm #(if (and (tagged-literal? %) (-> % .-tag (= 'tooling/eval)))
                (pr-str (.-form %))
                (str "'" (pr-str %)))
        code (str "(" fun " '"
                  (pr-str (norm-evt (.-target e)))
                  " '" (pr-str @state)
                  " " (->> additional-args (map norm) (str/join " "))
                  ")")
        res (eql {:text/contents code} [:repl/result])]
    (p/then res (fn [response]
                  (reset! state (-> response :repl/result :result))))))

(defn- prepare-fn [eql fun state repl]
  (fn [ & args]
    (if (-> args first edn?)
      (fn [e] (run-evt-fun! eql e fun state repl args))
      (run-evt-fun! eql (first args) fun state repl []))))

(defn- bindings-for [editor-state eql state fns repl]
  (let [binds (coll/map-values (fn [v] (fn [ & args]
                                         (p/do! (apply v args))
                                         nil))
                               (int/debug-bindings editor-state))
        binds (assoc binds
                     '?state @state
                     '?state-atom state
                     'eql eql
                     'eval #(tagged-literal 'tooling/eval %))]
    (->> fns
         (map (fn [[f-name f-body]] [(->> f-name name (str "?") symbol)
                                     (prepare-fn eql f-body state repl)]))
         (into binds))))

(defn- treat-error [hiccup]
  (let [d (. js/document createElement "div")]
    (rdom/render hiccup d)
    hiccup))

;; TODO: Migrate this to a better place!
(defn- prepare-new-eql [editor-state]
  (let [eql (-> @editor-state :editor/features :eql)
        cached-result (eql [:editor/data :config/repl-kind
                            :config/eval-as :config/project-paths
                            :repl/evaluators])]
    (fn q
      ([query] (q {} query))
      ([seed query]
       (p/let [original-seed cached-result]
         (eql (merge original-seed seed) query))))))

(defn- render-interactive [{:keys [state html fns] :as edn} repl editor-state]
  (let [state (r/atom state)
        code (pr-str html)
        eql (prepare-new-eql editor-state)
        html (fn [state]
               (try
                 (-> {:code code
                      :bindings (bindings-for editor-state eql state fns repl)
                      :editor-state editor-state}
                     int/evaluate-code
                     pinkie/tag-inject
                     treat-error)
                 (catch :default e
                   (.log js/console e)
                   [:div.error "Can't render this code - " (pr-str e)])))]
    [html state]))

(defrecord Interactive [edn repl editor-state]
  proto/Renderable
  (as-html [_ ratom _]
    (render-interactive edn repl editor-state)))
