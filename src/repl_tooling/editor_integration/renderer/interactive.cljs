(ns repl-tooling.editor-integration.renderer.interactive
  (:require [reagent.core :as r]
            [promesa.core :as p]
            [reagent.dom :as rdom]
            [paprika.collection :as coll]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.renderer.protocols :as proto]
            [repl-tooling.ui.pinkie :as pinkie]
            [repl-tooling.editor-integration.interpreter :as int]

            [reagent.dom.server :as r-server]
            [reagent.impl.component :as r-component]
            [reagent.impl.protocols :as r-proto]
            [reagent.impl.template :as t]))

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

(defn error-boundary [comp]
  (r/create-class
    {:constructor (fn [this props]
                    (set! (.-state this) #js {:error nil}))
     :component-did-catch (fn [this e info])
     :get-derived-state-from-error (fn [error] #js {:error error})
     :render (fn [this]
               (r/as-element
                (if-let [error (.. this -state -error)]
                  [error-boundary
                   [:div.error.rows
                    [:div.title "Something went wrong."]
                    [:div.space]
                    [:div (pr-str error)]]]
                  comp)))}))

(defonce orig-wrap-render r-component/wrap-render)

(def ^:private wrappings (atom ()))
(defn- wrap-render-into-tries [errors]
  (swap! wrappings conj ::wrapped)
  (set! r-component/wrap-render (fn [c compiler]
                                  (try
                                    (orig-wrap-render c compiler
                                                      val)
                                    (catch :default e
                                      (swap! errors conj [c e])
                                      (r/create-element "div"))))))

(defn- check-errors [hiccup]
  (try
    (let [errors (atom [])]
      (wrap-render-into-tries errors)
      (r-server/render-to-static-markup hiccup)
      @errors)
    (catch :default e
      [e])
    (finally
      (p/do!
       (p/delay 500)
       (when (empty? (swap! wrappings rest))
         (set! r-component/wrap-render orig-wrap-render))))))

(defn- render-interactive [{:keys [state html fns] :as edn} repl editor-state]
  (let [state (r/atom state)
        code (pr-str html)
        eql (prepare-new-eql editor-state)
        html (fn [state]
               (-> {:code code
                    :bindings (bindings-for editor-state eql state fns repl)
                    :editor-state editor-state}
                   int/evaluate-code
                   pinkie/tag-inject))]
    (if-let [errors (-> [html state] check-errors not-empty)]
      [:div.rows
       [:div.error.title "Error parsing the code to render the custom view"]
       [:div.space]
       [:div.pre (-> errors first pr-str)]]
      [error-boundary
        [html state]])))
#_
(r/set-default-compiler! t/default-compiler*)

#_
(let [reag (fn [tag v compiler]
             ; (prn :TAG tag :V v)
             (try
               (t/reag-element tag v compiler)
               (catch :default e
                 (prn :AN-ERROR e))))
      compiler
      (let [id "My-COMPILER"]
        (reify r-proto/Compiler
          ;; This is used to as cache key to cache component fns per compiler
          (get-id [this] id)
          (parse-tag [this tag-name tag-value]
                     (try
                       ; (prn :WAT3)
                       (t/cached-parse this tag-name tag-value)
                       (catch :default e
                         (prn :ERROR3! e)
                         (r/as-element [:div.error "ERROR HERE"]))))
          (as-element [this x]
                      (try
                        ; (prn :WAT x)
                        (t/as-element this x reag)
                        (catch :default e
                          (prn :ERROR! e)
                          (r/as-element [:div.error "ERROR HERE"]))))
          (make-element [this argv component jsprops first-child]
                      (try
                        ; (prn :WAT2)
                        (t/make-element this argv component jsprops first-child)
                        (catch :default e
                          (prn :ERROR2! e)
                          (r/as-element [:div.error "ERROR HERE"]))))))]
  (r/set-default-compiler! compiler)
  (rdom/render [html state] (js/document.createElement "div")))

#_
(int/evaluate-code
 {:code code
  :bindings (bindings-for editor-state eql state {} nil)
  :editor-state editor-state})

#_
(let [old r-component/wrap-render
      errors (atom [])]
  (with-redefs [r-component/wrap-render (fn [c compiler]
                                          (try
                                            (old c compiler)
                                            (catch :default e
                                              (swap! errors conj [c e])
                                              [])))]
    (r-server/render-to-string [html state])))

(defrecord Interactive [edn repl editor-state]
  proto/Renderable
  (as-html [_ ratom _]
    (render-interactive edn repl editor-state)))
