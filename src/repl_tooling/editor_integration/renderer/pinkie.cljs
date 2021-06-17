(ns repl-tooling.editor-integration.renderer.pinkie
  (:require [promesa.core :as p]
            [clojure.string :as str]
            [reagent.core :as r]
            [repl-tooling.ui.pinkie :as pinkie]
            [pinkgorilla.ui.jsrender :as jsrender]
            ["highlight.js" :as highlight]
            ["commonmark" :refer [Parser HtmlRenderer]]
            ["path" :refer [dirname join]]
            ["fs" :refer [watch readFile existsSync]]
            ["ansi_up" :default Ansi]))

(defn- norm-reagent-fn [fun]
  (fn [ & args]
    (let [empty (js/Object.)
          state (r/atom empty)
          render (fn [ state & args]
                   (if (= empty @state)
                     (do
                       (p/let [res (apply fun args)]
                         (reset! state res))
                       [:div.repl-tooling.icon.loading])
                     @state))]
      (apply vector render state args))))

(defn register-reagent [keyword fun]
  (pinkie/register-tag keyword (norm-reagent-fn fun)))

(defn- norm-pinkie-fn [fun]
  (fn [ & args]
    [jsrender/render-js
     {:f (fn [dom args]
           (let [div (.createElement js/document "div")
                 upd (fn [elem]
                       (try (.removeChild dom div) (catch :default _))
                       (.appendChild dom elem))
                 elem (apply fun (js->clj args))]
             (.. div -classList (add "repl-tooling" "icon" "loading"))
             (.appendChild dom div)
             (if (instance? js/Promise elem)
               (.then elem upd)
               (upd elem))))
      :data args}]))

(defn register-tag [keyword fun]
  (pinkie/register-tag keyword (norm-pinkie-fn fun)))

(defonce ^:private ansi (new Ansi))
(defn ansi-tag [attrs & txts]
  (let [[attrs txts] (if (map? attrs)
                       [attrs txts]
                       [{} (cons attrs txts)])
        attrs (merge {:class "pre block"} attrs)]
    [:div (assoc attrs
                 :dangerouslySetInnerHTML
                 #js {:__html (. ansi ansi_to_html (apply str txts))})]))

(defn code-tag [ param & body]
  (let [[params body] (if (map? param)
                        [param body]
                        [nil (cons param body)])]

    [:pre (assoc params :class "hljs")
     [:code {:dangerouslySetInnerHTML
             {:__html
              (.-value
               (highlight/highlight (str/join "" body)
                                    #js {:language "clojure"
                                         :ignoreIllegals true}))}}]]))

(defn markdown-tag [ & body]
  (let [parser (new Parser)
        render (new HtmlRenderer)
        body (str/join "" body)
        div (js/document.createElement "div")]
    (.. div -classList (add "rows"))
    (aset div "innerHTML" (->> body (.parse parser) (.render render)))
    (doseq [n (.querySelectorAll div "pre code")]
      (highlight/highlightElement n)
      (.. n -parentElement -classList (add "hljs")))
    div))
