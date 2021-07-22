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

#_
(set! r-component/wrap-render orig-wrap-render)
#_
(let [asyncs (atom {})
      errors (atom [])]
  (set! r-component/wrap-render
    (fn [c compiler]
      (prn :WAT?)
      (let [id (gensym "elem-")]
        (prn :CATCHING id)
        (swap! asyncs assoc id (p/deferred))
        (try
          (let [val (orig-wrap-render c compiler)]
            (swap! asyncs update id p/resolve! val)
            val)
          (catch :default e
            (swap! errors conj [c e])
            (swap! asyncs update id p/resolve! e)
            []))))))



#_
(let [tr (js/require "react-test-renderer")]
  (.create tr (r/as-element [:div 10])))

#_
(let [React (js/require "react")]
  (.isValidElement React
                   (r/as-element [html state])))

(defn- wrap-render-into-tries [errors]
  (set! r-component/wrap-render (fn [c compiler]
                                  ; (let [id (gensym "elem-")]
                                    ; (prn :TREAT id)
                                    ; (swap! asyncs assoc id (p/deferred))
                                    (try
                                      ; (prn :TRY)
                                      (orig-wrap-render c compiler
                                        ; (swap! asyncs update id p/resolve! val)
                                        ; (prn :OK)
                                        val)
                                      (catch :default e
                                        ; (prn :ERROR)
                                        (swap! errors conj [c e])
                                        ; (swap! asyncs update id p/resolve! e)
                                        ; (prn :PLACEHOLDER)
                                        (r/create-element "div"))))))
;
; (r-server/render-to-string #_[html state]
; ; (r-component/wrap-funs
;  (r-proto/as-element
;   t/default-compiler*
;   [html state])
;  t/default-compiler*)
;
(defn- check-errors [hiccup]
  (try
    (let [;asyncs (atom {})
           errors (atom [])]
      ; errors (atom [])]
      ; (set! r-component/wrap-render (fn [c compiler]
      ;                                 (let [id (gensym "elem-")]
      ;                                   (prn :TREAT id)
      ;                                   (swap! asyncs assoc id (p/deferred))
      ;                                   (try
      ;                                     (prn :TRY)
      ;                                     (let [val (orig-wrap-render c compiler)]
      ;                                       (swap! asyncs update id p/resolve! val)
      ;                                       (prn :OK)
      ;                                       val)
      ;                                     (catch :default e
      ;                                       (prn :ERROR)
      ;                                       (swap! errors conj [c e])
      ;                                       (swap! asyncs update id p/resolve! e)
      ;                                       (r/create-element "div"))))))
      (prn :SET! r-component/wrap-render)
      (wrap-render-into-tries errors)
      (prn :SET! r-component/wrap-render)
      (prn :RES)
      (r-server/render-to-static-markup hiccup)
      (prn :OK! r-component/wrap-render)
      ; #_
      ; (-> (vals @asyncs)
      ;     (doto prn)
      ;     p/all
      ;     (doto prn)
      ;     (p/finally #(p/do!
      ;                  (prn :UNSET)
      ;                  (p/delay 1000)
      ;                  (prn :UNSET2)
      ;                  (set! r-component/wrap-render orig-wrap-render))))
      (prn :RET-ERRORS)
      @errors)
    (catch :default e
      [e])
    (finally
      (p/do!
       ()
       (set! r-component/wrap-render orig-wrap-render)))))

; (macroexpand-1
;  '(with-redefs [r-component/wrap-render (fn [a] a)]
;     (r-component/wrap-render 1)))
; #_
; (check-errors [:div [:div 10]])
;
;
; #_
; (check-errors [html state])

; (try
;   (html state)
;   (catch :default e e))
; (r-component/cljsify html t/default-compiler*)

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
                   ; treat-error))]
    (def editor-state editor-state)
    (def state state)
    (def code code)
    (def eql eql)
    (def html html)
    ; (try)
    ; (let [errors (check-errors [html state])])
      ; (prn :ERRORS errors)
    ; (js/setTimeout)
    (try
      (prn :ERRORS (check-errors [html state]))
      (catch :default e
        (prn :ERROR :TIMEOUT)))

    1000
    [error-boundary
     ; [:div "WAT"
     ;  (if (seq errors)
     ;    [:div.error.rows
     ;     [:div.title "Can't render this code"]
     ;     [:div.space]
     ;     [:div (pr-str errors)]]
        [:div "OK"]]))
      ; (r/with-let [curr-state (r/atom :loading)]
      ;   (p/then errors #(do
      ;                     (prn :SWAPPING-WITH %)
      ;                     (reset! curr-state %)))
      ;   (case @curr-state
      ;     :loading [:div "Loading..."]
      ;     [] [:div "OK"]
      ;   ; (if (seq errors)
      ;     [:div.error.rows
      ;      [:div.title "Can't render this code"]
      ;      [:div.space]
      ;      [:div (pr-str errors)]])))))
      ;
      ;     ; #_
          ; [error-boundary [html state]]))
    ; (catch :default e
    ;   (pr-str :ERROR e)
    ;   [:div.error.rows
    ;    [:div.title "Can't render this code"]
    ;    [:div.space]
    ;    [:div (pr-str e)]])))

#_
(int/evaluate-code
 {:code code
  :bindings (bindings-for editor-state eql state {} nil)
  :editor-state editor-state})

; #_
; (try
;   (let [div (js/document.createElement "div")]
;     (rdom/render [error-boundary [html state]] div))
;   (catch :default e
;     (prn :THAT-AN-ERROR)))
  ; div)
; (set! *assert* false)
; t/default-compiler*

; #_
; (let [reag (fn [tag v compiler]
;              ; (prn :TAG tag :V v)
;              (try
;                (t/reag-element tag v compiler)
;                (catch :default e
;                  (prn :AN-ERROR e))))
;       compiler
;       (let [id "My-COMPILER"]
;         (reify r-proto/Compiler
;           ;; This is used to as cache key to cache component fns per compiler
;           (get-id [this] id)
;           (parse-tag [this tag-name tag-value]
;                      (try
;                        ; (prn :WAT3)
;                        (t/cached-parse this tag-name tag-value)
;                        (catch :default e
;                          (prn :ERROR3! e))))
;           (as-element [this x]
;                       (try
;                         ; (prn :WAT x)
;                         (t/as-element this x reag)
;                         (catch :default e
;                           (prn :ERROR! e))))
;           (make-element [this argv component jsprops first-child]
;                       (try
;                         ; (prn :WAT2)
;                         (t/make-element this argv component jsprops first-child)
;                         (catch :default e
;                           (prn :ERROR2! e))))))]
;   (t/set-default-compiler! compiler)
;   (rdom/render [html state] (js/document.createElement "div")))
;
;
;
; #_
; (t/hiccup-element
;  [html state]
;  (t/create-compiler {:function-components (partial prn :FUNCTION)
;                      :parse-tag (partial prn :TAG)}))
;
; #_
; (reagent.impl.component/do-render
;  [:div "foo"]
;  (t/create-compiler {:function-components (partial prn :FUNCTION)
;                      :parse-tag (partial prn :TAG)}))

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
; (r-server/render-to-string (r/as-element [:div.title "FOO"]))
; (r-server/render-to-string [:div "FOO"])
; (reagent.dom.server/render-to-static-markup)
;
; (rdom/dom-node)
;

(defrecord Interactive [edn repl editor-state]
  proto/Renderable
  (as-html [_ ratom _]
    (render-interactive edn repl editor-state)))
