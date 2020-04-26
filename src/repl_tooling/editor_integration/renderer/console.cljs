(ns repl-tooling.editor-integration.renderer.console
  (:require [reagent.core :as r]
            [promesa.core :as p]
            [repl-tooling.editor-integration.renderer :as render]
            ["ansi_up" :default Ansi]))

(defonce out-state
  (r/atom []))

(defn- rendered-content [parsed-ratom]
  (let [error? (-> parsed-ratom meta :error)]
    [:div {:class ["result" "repl-tooling" (when error? "error")]}
     [render/view-for-result parsed-ratom]]))

(defonce ansi (new Ansi))
(defn- cell-for [[out-type object] idx]
  (let [kind (out-type {:stdout :output :stderr :err :result :result})
        icon (out-type {:stdout "icon-quote" :stderr "icon-alert" :result "icon-code"})]
    [:div.cell {:key idx}
     [:div.gutter [:span {:class ["icon" icon]}]]
     (if (= out-type :result)
       [:div.content [rendered-content object]]
       (let [html (. ansi ansi_to_html object)]
         [:div.content [:div {:class kind :dangerouslySetInnerHTML #js {:__html html}}]]))]))

(defn console-view [ & classes]
  [:div {:tabIndex 1 :class (conj classes "repl-tooling" "console")}
   [:<> (map cell-for @out-state (range))]])

(defonce div (. js/document createElement "div"))

(defn- console-elem []
  (. div (querySelector "div.repl-tooling")))

(defn all-scrolled? []
  (let [console (console-elem)
        console-height (.-scrollHeight console)
        parent-height (.. div -clientHeight)
        offset (- console-height parent-height)
        scroll-pos (.-scrollTop console)]
    (>= scroll-pos offset)))

(defn scroll-to-end! [scrolled?]
  (let [console (console-elem)]
    (when @scrolled?
      (set! (.-scrollTop console) (.-scrollHeight console)))))

(defn clear []
  (reset! out-state []))

(defn- append-text [stream text]
  (let [[old-stream old-text] (peek @out-state)]
    (if (= old-stream stream)
      (swap! out-state #(-> % pop (conj [stream (str old-text text)])))
      (swap! out-state conj [stream text]))))

(defn stdout [txt]
  (append-text :stdout txt))

(defn stderr [txt]
  (append-text :stderr txt))

(defn result [parsed-result parse-fn]
  (p/let [res (parse-fn parsed-result)]
    (swap! out-state conj [:result res])))
