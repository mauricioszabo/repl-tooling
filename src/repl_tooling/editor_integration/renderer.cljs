(ns repl-tooling.editor-integration.renderer
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [clojure.walk :as walk]
            [repl-tooling.editor-helpers :as helpers]))

(defprotocol Renderable
  (as-html [this ratom root?]))

(defprotocol Parseable
  (as-renderable [self repl]))

#_
((:more-fn new-idx) (:repl new-idx) prn)
; (more-fn (:repl new-idx) prn)

(declare ->indexed)
(defrecord Indexed [open obj close kind expanded? more-fn repl]
  Renderable
  (as-html [_ ratom root?]
    ; (prn [:MORE more-fn])
    ; (def more-fn more-fn)
    (let [reset-atom #(let [new-idx (->indexed % repl)]
                        (def obj obj)
                        (def repl repl)
                        (def % %)
                        (def ratom ratom)
                        (def new-idx new-idx)
                        (swap! ratom
                               (fn [indexed]
                                 (let [new-obj (vec (concat obj (drop (count obj) (:obj new-idx))))
                                       new-more (:more-fn new-idx)]
                                   (assoc indexed
                                          :obj new-obj
                                          :more-fn new-more
                                          :repl (:repl new-idx))))))
          inner-parsed (cond-> (mapv #(as-html (deref %) % false) obj)
                               more-fn
                               (conj [:a {:href "#"
                                          :on-click (fn [e]
                                                      (.preventDefault e)
                                                      (more-fn repl reset-atom))}
                                      "..."]))
          inner (->> inner-parsed
                     (interpose [:div {:class "whitespace"} ", "])
                     (map #(with-meta %2 {:key %1}) (range)))]

      [:div {:class ["row" kind]}
       [:div {:class ["coll" kind]}
        (when root?
          [:a {:class ["chevron" (if expanded? "opened" "closed")] :href "#"
               :on-click (fn [e]
                           (.preventDefault e)
                           (swap! ratom update :expanded? not))}])
        [:div {:class "delim open"} open]
        [:div {:class "inner"} inner]
        [:div {:class "delim close"} close]]

       (when (and root? expanded?)
         [:div {:class "children"}
          [:<>
           (cond-> (mapv #(as-html (deref %) % true) obj)

                   more-fn (conj [:a {:href "#"
                                      :on-click (fn [e]
                                                  (.preventDefault e)
                                                  (more-fn repl reset-atom))}
                                  "..."])

                   :then (->> (map (fn [i e] [:div {:key i :class "row"} e]) (range))))]])])))

(defrecord Leaf [obj]
  Renderable
  (as-html [_ _ _]
    (let [tp (cond
               (string? obj) "string"
               (number? obj) "number"
               (boolean? obj) "bool"
               (nil? obj) "nil"
               :else "other")]
      [:div {:class tp} (pr-str obj)])))

(defn- ->indexed [obj repl]
  (let [more-fn (eval/get-more-fn obj)
        children (mapv #(as-renderable % repl) (eval/without-ellision obj))]
    (cond
      (vector? obj) (->Indexed "[" children "]" "vector" false more-fn repl)
      ; (set? obj) (->Indexed "#{" (vec children) "}" "set" false more-fn repl)
      ; (map? obj) (->Indexed "{" (vec children) "}" "map" false more-fn repl)
      (seq? obj) (->Indexed "(" children ")" "list" false more-fn repl))))

(extend-protocol Parseable
  default
  (as-renderable [obj repl]
    (r/atom
      (cond
        (vector? obj) (->indexed obj repl)
        ; (set? obj) (->indexed obj repl)
        ; (map? obj) (->indexed obj repl)
        (seq? obj) (->indexed obj repl)
        :else (->Leaf obj)))))

(defn parse-result
  "Will parse a result that comes from the REPL in a r/atom so that
it'll be suitable to be rendered with `view-for-result`"
  [result repl]
  (let [parsed (helpers/parse-result result)
        res (:result parsed)]
    (if res
      (as-renderable res repl)
      (with-meta (as-renderable (:error result) repl) {:error true}))))

(defn view-for-result
  "Renders a view for a result. If it's an error, will return a view
suitable for error backtraces. If it's a success, will return a success
view. Expects a r/atom that comes from `parse-result`"
  [state repl]
  (as-html @state state true))
