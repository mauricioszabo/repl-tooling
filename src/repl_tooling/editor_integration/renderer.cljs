(ns repl-tooling.editor-integration.renderer
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [clojure.walk :as walk]
            [repl-tooling.editor-helpers :as helpers]))

(defprotocol Renderable
  (as-html [this ratom root?])
  (as-text [this ratom root?]))

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
        vec
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
         (when root? "...")])]
     (when (and root? expanded?)
       [:div {:class "row children"}
        [as-html @attributes-atom attributes-atom true]])]))

(defn- assert-root [txt]
  (if (-> txt first (= :row))
    txt
    [:row txt]))

(declare ->indexed)
(defn- reset-atom [repl ratom obj result]
  (let [new-idx (->indexed result repl)]
    (swap! ratom
           (fn [indexed]
             (assoc indexed
                    :obj (vec (concat obj (:obj new-idx)))
                    :more-fn (:more-fn new-idx))))))

(defrecord Indexed [open obj close kind expanded? more-fn repl]
  Renderable
  (as-html [_ ratom root?]
           (let [a-for-more [:a {:href "#"
                                 :on-click (fn [e]
                                             (.preventDefault e)
                                             (more-fn repl false #(reset-atom repl ratom obj %)))}
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
                                        (parse-inner-for-map obj more-fn a-for-more)
                                        (parse-inner-root obj more-fn a-for-more))]
               [:div {:class "delim closing"} close]]

              (when (and root? expanded?)
                [:div {:class "children"}
                 [:<>
                  (cond-> (mapv #(as-html (deref %) % true) obj)
                          more-fn (conj a-for-more)
                          :then (->> (map (fn [i e] [:div {:key i :class "row"} e]) (range))))]])]))

  (as-text [_ ratom root?]
           (let [children (map #(as-text @% % false) obj)
                 toggle #(do (swap! ratom update :expanded? not) (%))
                 extract-map #(-> % second (str/replace #"^\[" "") (str/replace #"\]$" ""))
                 txt (if (= "map" kind)
                       [:text (->> obj
                                   (map #(extract-map (as-text @% % false)))
                                   (str/join ", "))]
                       [:text (->> children (map second) (str/join " "))])
                 more-callback (fn [callback]
                                 (more-fn repl false
                                          #(do
                                             (reset-atom repl ratom obj %)
                                             (callback))))
                 complete-txt (delay (if more-fn
                                       (update txt 1 #(str open % " ..." close))
                                       (update txt 1 #(str open % close))))
                 root-part (delay [:expand (if expanded? "-" "+") toggle])
                 rows (cond
                        (not root?) @complete-txt
                        more-fn [:row
                                 @root-part
                                 (update txt 1 #(str open %))
                                 [:button "..." more-callback]
                                 [:text close]]
                        :else [:row @root-part @complete-txt])]
             (if expanded?
               (cond-> (apply conj rows (map #(assert-root (as-text @% % true)) obj))
                       more-fn (conj [:row [:button "..." more-callback]]))
               rows))))

(defrecord Leaf [obj]
  Renderable
  (as-html [_ _ _]
    (let [tp (cond
               (string? obj) "string"
               (number? obj) "number"
               (boolean? obj) "bool"
               (nil? obj) "nil"
               :else "other")]
      [:div {:class tp} (pr-str obj)]))
  (as-text [_ _ _]
    [:text (pr-str obj)]))

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
     [:span (-> string eval/without-ellision pr-str (str/replace #"\"$" ""))]
     (when-let [get-more (eval/get-more-fn string)]
       [:a {:href "#"
            :on-click (fn [e]
                        (.preventDefault e)
                        (get-more repl #(swap! ratom assoc :string %)))}
         "..."])
     "\""])
  (as-text [_ ratom root?]
    (if root?
      [:row
       [:text (-> string eval/without-ellision pr-str (str/replace #"\"$" ""))]
       [:button "..." #(let [f (eval/get-more-fn string)]
                         (f repl (fn [obj]
                                   (if (string? obj)
                                     (reset! ratom (->Leaf obj))
                                     (swap! ratom assoc :string obj))
                                   (%))))]
       [:text "\""]]
      [:text (pr-str string)])))

(defrecord Tagged [tag subelement]
  Renderable
  (as-html [_ ratom root?]
    [:div {:class "tagged"} [:span {:class "tag"} tag]
     [as-html @subelement subelement root?]]))

(defrecord IncompleteObj [incomplete repl]
  Renderable
  (as-html [_ ratom root?]
    (let [more (eval/get-more-fn incomplete)]
      [:div {:class "incomplete-obj"}
       [:a {:href "#" :on-click (fn [e]
                                  (.preventDefault e)
                                  (more repl #(reset! ratom @(as-renderable % repl))))}
        "..."]])))

(defn- link-for-more-trace [repl ratom more-trace more-str]
  (cond
    more-trace
    (fn [e]
      (.preventDefault e)
      (more-trace repl #(reset! ratom %)))

    more-str
    (fn [e]
      (.preventDefault e)
      (more-str repl #(swap! ratom assoc 2 %)))))

(defn- to-trace-row [repl ratom idx trace]
  (let [[class method file row] trace
        link-for-more (link-for-more-trace repl
                                           (r/cursor ratom [:obj :trace idx])
                                           (eval/get-more-fn trace)
                                           (eval/get-more-fn file))
        clj-file? (re-find #"\.clj?$" (str file))]
    (cond
      link-for-more
      [:div {:key idx :class ["row" "incomplete"]}
       [:div "in " [:a {:href "#" :on-click link-for-more} "..."]]]

      (not= -1 row)
      [:div {:key idx :class ["row" (if clj-file? "clj-stack" "stack")]}
       [:div
        "in "
        [:span {:class "class"} (cond-> (str class) clj-file? demunge)]
        (when-not clj-file? [:span {:class "method"} "."
                             method])
        [:span {:class "file"} " (" file ":" row ")"]]])))

(defrecord ExceptionObj [obj add-data repl]
  Renderable
  (as-html [_ ratom root?]
    (let [{:keys [type message trace]} obj]
      [:div {:class "exception row"}
       [:div {:class "description"}
        [:span {:class "ex-kind"} (str type)] ": " [:span {:class "ex-message"} message]]
       (when add-data
         [:div {:class "children additional"}
          [as-html @add-data add-data root?]])
       (when root?
         [:div {:class "children"}
          (doall
            (map (partial to-trace-row repl ratom)
                 (range)
                 (eval/without-ellision trace)))
          (when-let [more (eval/get-more-fn trace)]
            [:a {:href "#" :on-click (fn [e]
                                       (.preventDefault e)
                                       (more repl #(swap! ratom assoc-in [:obj :trace] %)))}
             "..."])])])))

(extend-protocol Parseable
  helpers/Error
  (as-renderable [self repl]
    (let [add-data (some-> self :add-data not-empty (as-renderable repl))]
      (r/atom (->ExceptionObj self add-data repl))))

  helpers/IncompleteObj
  (as-renderable [self repl]
    (r/atom (->IncompleteObj self repl)))

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
      (let [error (:error parsed)
            ex (cond-> error
                       (:ex error) :ex
                       (->> error :ex (instance? helpers/Browseable)) :object)]
        (with-meta (as-renderable ex repl) {:error true})))))

(defn view-for-result
  "Renders a view for a result. If it's an error, will return a view
suitable for error backtraces. If it's a success, will return a success
view. Expects a r/atom that comes from `parse-result`"
  [state]
  [as-html @state state true])

(defn txt-for-result
  "Renders a view for a result, but in textual format. This view will be
in a pseudo-hiccup format, like so:
[:row [:expand \"+\" some-fn]
      [:text \"(1 2 3 4 5 6\"]
      [:button \"...\" some-fn]
      [:text \")\"]]

Where :row defines a row of text, :text a fragment, :button a text that's
associated with some data (to be able to ellide things) and :expand is to
make a placeholder that we can expand (+) or collapse (-) the structure"
  [state]
  (assert-root (as-text @state state true)))

(defn- parse-funs [funs last-elem curr-text elem]
  (let [txt-size (-> elem (nth 1) count)
        curr-row (count curr-text)
        fun (peek elem)]
    (reduce (fn [funs col] (assoc funs [last-elem col] fun))
            funs (range curr-row (+ curr-row txt-size)))))

(defn- parse-elem [position lines funs depth]
  (let [[elem text function] position
        last-elem (-> lines count dec)
        indent (->> depth (* 2) range (map (constantly " ")) (apply str) delay)
        last-line (peek lines)
        curr-text (if (empty? last-line)
                    @indent
                    (str last-line " "))]
    (case elem
      :row (recur (rest position) (conj lines "") funs (inc depth))
      :text [(assoc lines last-elem (str curr-text text)) funs]
      :button [(assoc lines last-elem (str curr-text text))
               (parse-funs funs last-elem curr-text position)]
      :expand [(assoc lines last-elem (str curr-text text " "))
               (parse-funs funs last-elem curr-text position)]
      (reduce (fn [[lines funs] position] (parse-elem position lines funs depth))
              [lines funs] position))))

(defn repr->lines [repr]
  (parse-elem repr [] {} -1))
