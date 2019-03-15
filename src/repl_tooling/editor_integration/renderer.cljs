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

(defn- parse-inner-root [objs more-fn a-for-more]
  (let [inner (cond-> (mapv #(as-html (deref %) % false) objs)
                      more-fn (conj a-for-more))]
    (->> inner
         (interpose [:span {:class "whitespace"} " "])
         (map #(with-meta %2 {:key %1}) (range)))))

(defn parse-inner-for-map [objs more-fn a-for-more]
  (let [sep (cycle [[:span {:class "whitespace"} " "]
                    [:span {:class "coll whitespace"} ", "]])
        inner (->> objs
                   (mapcat #(-> % deref :obj))
                   (map #(as-html (deref %) % false)))]
    (-> inner
        (interleave sep)
        butlast
        (cond-> more-fn (conj (second sep) a-for-more))
        (->> (map #(with-meta %2 {:key %1}) (range))))))

(defrecord ObjWithMore [obj-atom more-fn attributes-atom expanded? repl]
  Renderable
  (as-html [_ ratom root?]
    [:div {:class ["browseable"]}
     [:div {:class ["object"]}
      (as-html @obj-atom obj-atom root?)
      (when more-fn
        [:a {:href "#"
             :on-click (fn [e]
                         (.preventDefault e)
                         (more-fn repl #(swap! ratom assoc
                                               :more-fn nil
                                               :expanded? true
                                               :attributes-atom (as-renderable (:attributes %) repl))))}
         ; (reset! ratom (deref (as-renderable % repl))))))}
         (when root? "...")])]
     (when (and root? expanded?)
       [:div {:class "row children"}
        (as-html @attributes-atom attributes-atom true)])]))

(declare ->indexed)
(defrecord Indexed [open obj close kind expanded? more-fn repl]
  Renderable
  (as-html [_ ratom root?]
    (let [reset-atom #(let [new-idx (->indexed % repl)]
                        (swap! ratom
                               (fn [indexed]
                                 (assoc indexed
                                        :obj (vec (concat obj (:obj new-idx)))
                                        :more-fn (:more-fn new-idx)))))
          a-for-more [:a {:href "#"
                          :on-click (fn [e]
                                      (.preventDefault e)
                                      (more-fn repl false reset-atom))}
                      "..."]]

      [:div {:class ["row" kind]}
       [:div {:class ["coll" kind]}
        (when root?
          [:a {:class ["chevron" (if expanded? "opened" "closed")] :href "#"
               :on-click (fn [e]
                           (.preventDefault e)
                           (swap! ratom update :expanded? not))}])
        [:div {:class "delim opening"} open]
        [:div {:class "inner"} (if (#{"map"} kind)
                                 (parse-inner-for-map obj false a-for-more)
                                 (parse-inner-root obj more-fn a-for-more))]
        [:div {:class "delim closing"} close]]

       (when (and root? expanded?)
         [:div {:class "children"}
          [:<>
           (cond-> (mapv #(as-html (deref %) % true) obj)
                   more-fn (conj a-for-more)
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
      (set? obj) (->Indexed "#{" (vec children) "}" "set" false more-fn repl)
      (map? obj) (->Indexed "{" (vec children) "}" "map" false more-fn repl)
      (seq? obj) (->Indexed "(" children ")" "list" false more-fn repl))))

(defrecord IncompleteStr [string repl]
  Renderable
  (as-html [_ ratom root?]
    [:div {:class "string big"}
     (-> string eval/without-ellision pr-str (str/replace #"\"$" ""))
     (when-let [get-more (eval/get-more-fn string)]
       [:a {:href "#"
            :on-click (fn [e]
                        (.preventDefault e)
                        (get-more repl #(swap! ratom assoc :string %)))}
         "..."])
     "\""]))

(defrecord Tagged [tag subelement]
  Renderable
  (as-html [_ ratom root?]
    [:div {:class "tagged"} [:span {:class "tag"} tag]
     (as-html @subelement subelement root?)]))

(extend-protocol Parseable
  helpers/IncompleteStr
  (as-renderable [self repl]
    (r/atom (->IncompleteStr self repl)))

  helpers/Browseable
  (as-renderable [self repl]
    (let [{:keys [object attributes]} self]
      (r/atom (->ObjWithMore (as-renderable object repl)
                             (eval/get-more-fn self)
                             (as-renderable attributes repl)
                             false
                             repl))))

  helpers/WithTag
  (as-renderable [self repl]
    (let [tag (helpers/tag self)
          subelement (-> self helpers/obj (as-renderable repl))]
      (r/atom (->Tagged tag subelement))))

  default
  (as-renderable [obj repl]
    (r/atom
      (cond
        (coll? obj) (->indexed obj repl)
        :else (->Leaf obj)))))

(defn parse-result
  "Will parse a result that comes from the REPL in a r/atom so that
it'll be suitable to be rendered with `view-for-result`"
  [result repl]
  (let [parsed (helpers/parse-result result)]
    (if (contains? parsed :result)
      (as-renderable (:result parsed) repl)
      (with-meta (as-renderable (:error result) repl) {:error true}))))

(defn view-for-result
  "Renders a view for a result. If it's an error, will return a view
suitable for error backtraces. If it's a success, will return a success
view. Expects a r/atom that comes from `parse-result`"
  [state repl]
  [as-html @state state true])
