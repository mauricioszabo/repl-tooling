(ns repl-tooling.editor-integration.renderer.interactive
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.renderer.protocols :as proto]
            [pinkgorilla.ui.pinkie :as pinkie]
            [pinkgorilla.ui.jsrender :as jsrender]
            [sci.core :as sci]
            [repl-tooling.editor-integration.configs :as configs]))
            ; ["nomnoml" :as n]))

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

(defn- treat-error [hiccup]
  (let [d (. js/document createElement "div")]
    (rdom/render hiccup d)
    hiccup))

(defn- render-interactive [{:keys [state html fns] :as edn} repl editor-state]
  (let [state (r/atom state)
        code (pr-str html)
        html (fn [state]
               (try
                 (-> {:code code
                      :bindings (bindings-for state fns repl)
                      :editor-state editor-state}
                     configs/evaluate-code
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

; (defn render-counter [_]
;   (let [f (fn [dom _]
;             (prn :WAT?)
;             (let [d (. js/document createElement "div")]
;               (aset d "innerText" " WAT Hello")
;               (.appendChild dom d)))]
;     [jsrender/render-js {:f f :data []}]))
;
; (pinkie/register-tag :p/counter render-counter)
; (pinkie/register-tag :p/strike (fn [ & args]
;                                  (prn :ARGS args)
;                                  [:div " LOL"]))
; (println "{:html [:div (flatten `(defn render-counter [_]\n  (let [f (fn [dom _]\n            (prn :WAT?)\n            (let [d (. js/document createElement \"div\")]\n              (aset d \"innerText\" \" WAT Hello\")\n              (.appendChild dom d)))]\n    [jsrender/render-js {:f f :data []}])))]}")
; (->> `(defn render-counter [_]
;         (let [f (fn [dom _]
;                   (prn :WAT?)
;                   (let [d (. js/document createElement "div")]
;                     (aset d "innerText" " WAT Hello")
;                     (.appendChild dom d)))]
;           [jsrender/render-js {:f f :data []}]))
;      flatten
;      (filter symbol?)
;      (remove #(#{"repl-tooling.editor-integration.renderer.interactive"
;                  "clojure.core"
;                  "cljs.core"}
;                 (namespace %)))
;      (remove #(str/starts-with? (str %) "."))
;      (group-by namespace)
;      (mapcat (fn [[ns names]] (map #(vector ns (name %)) names)))
;      vec)
; (defn- parse-nomno [data-struct]
;   (if (empty? data-struct)
;     "[*]"
;     (reduce (fn [acc [k v]]
;               (str acc "[" k "]->[" v "]\n"))
;             ""
;             data-struct)))
;
; (defn render-diagram [dom nomno]
;   (let [div (. js/document createElement "div")
;         svg (n/renderSvg (cond-> nomno (not (string? nomno)) parse-nomno))]
;     (aset div "innerHTML" svg)
;     (.appendChild dom div)))
;
; (defn render-diagram [dom nomno]
;    (let [div (. js/document createElement "div")
;          svg (n/renderSvg (cond-> nomno (not (string? nomno)) parse-nomno))]
;      (aset div "innerHTML" svg)
;      (.appendChild dom div)))

; (pinkie/register-tag :p/diag #(vector jsrender/render-js {:f render-diagram :data %}))

; (parse-nomno
;  [[:foo 10]
;   [:bar 20]])
;
; #_
; {:html [:p/diag [[:foo 10]
;                  [:bar 20]]]}
; #_
; {:html [:p/diag "[REPLs]->[Clojure]
; [REPLs]->[ClojureScript]"]}
;

; (def src "[nomnoml] is -> [awesome]")
; (defn render []
;   (n/renderSvg src))
;
; (.log js/console
;       (render))

; console.log(nomnoml.renderSvg(src));

; (jsrender/jsrender)
; (jsrender/render-js)
; (BTW, too late here, almost going to sleep - that's why)
