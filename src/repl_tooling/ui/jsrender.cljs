(ns repl-tooling.ui.jsrender
  "jsrender is part of a jsrequire based module system to render data via javascipt inside
   reagent components. Typically custom ui renderers would use it:
   [jsrender custom-module data]"
  (:require
   [reagent.core :as reagent]
   [reagent.dom]
   [cljs-uuid-utils.core :as uuid]))

(defn error-boundary [_ #_comp]
  (let [error (reagent/atom nil)
        info (reagent/atom nil)]
    (reagent/create-class
     {:component-did-catch (fn [_ #_this _ #_e i]
                             (println "pinkie component did catch: " i)
                             (reset! info i))
      :get-derived-state-from-error (fn [e]
                                      (println "pinkie component get-derived-state-from-error: " e)
                                      (reset! error e)
                                      #js {})
      :reagent-render (fn [comp]
                        (if @error
                          [:div.error "Something went wrong."]
                          comp))})))

(defn info [s]
  (.log js/console s))

(defn render-function-impl
  [{:keys [f data]}]
  (let [uuid (uuid/uuid-string (uuid/make-random-uuid))]
    (reagent/create-class
     {:display-name "render-function"
      :reagent-render (fn [] [:div {:id uuid}])
      :component-did-mount (fn [this]
                             ;(info (str "jsrender init data: " data))
                             (f (reagent.dom/dom-node this) data))
          ;:component-did-update (fn [this]
          ;                        (run-script uuid data snippet))

 ;(let [[_ series-values] (reagent/argv this)]

      :component-will-update (fn [this [_ {:keys [f data]}]]
              ; with changing of parameters, re-render the component. (important for vega charts)
                               (info (str "jsrender new params: " data))
                               (f (reagent.dom/dom-node this) data))})))

(defn render-clj [data]
  [error-boundary
   [render-function-impl data]])

(defn ^{:category :pinkie}
  render-js
  "reagent component that renders a js function,
       calls
       parameters:
         f    the js render function
              gets js data
         data a clojure datastructure that will be converted to js
              before calling f"
  [{:keys [f data]}]
  (let [data-js {:f f :data (clj->js data)}]
    [render-clj data-js]))
