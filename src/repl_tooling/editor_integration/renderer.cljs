(ns repl-tooling.editor-integration.renderer
  (:require [reagent.core :as r]
            [promesa.core :as p]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.renderer.protocols :as proto]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.editor-integration.definition :as def]
            [repl-tooling.editor-integration.renderer.interactive :as int]
            [repl-tooling.editor-integration.commands :as cmds]
            ["source-map" :refer [SourceMapConsumer]]))

(defn- parse-inner-root [objs more-fn a-for-more]
  (let [inner (cond-> (mapv #(proto/as-html (deref %) % false) objs)
                      more-fn (conj a-for-more))]
    (->> inner
         (interpose [:span {:class "whitespace"} " "])
         (map #(with-meta %2 {:key %1}) (range)))))

(defn parse-inner-for-map [objs more-fn a-for-more]
  (let [sep (cycle [[:span {:class "whitespace"} " "]
                    [:span {:class "coll whitespace"} ", "]])
        inner (->> objs
                   (mapcat #(-> % deref :obj))
                   (map #(proto/as-html (deref %) % false)))]
    (-> inner
        (interleave sep)
        butlast
        vec
        (cond-> more-fn (conj (second sep) a-for-more))
        (->> (map #(with-meta %2 {:key %1}) (range))))))

(defn- assert-root [txt]
  (if (-> txt first (= :row))
    txt
    [:row txt]))

(declare txt-for-result)
;; FIXME: Checl why first-line-only? is not being used
(defn textual->text [elements first-line-only?]
  (let [els (cond->> elements first-line-only? (remove #(and (coll? %) (-> % first (= :row)))))]
    (->> els
         flatten
         (partition 2 1)
         (filter #(-> % first (= :text)))
         (map second)
         (apply str))))

(defn- copy-to-clipboard [ratom editor-state first-line-only?]
  (let [copy (-> @editor-state :editor/callbacks (:on-copy #()))]
    (-> ratom txt-for-result (textual->text first-line-only?) copy)))

(defn- obj-with-more-fn [more-fn ratom repl editor-state callback]
  (more-fn repl (fn [res]
                  (swap! ratom assoc
                         :more-fn nil
                         :expanded? true
                         :attributes-atom (proto/as-renderable (:attributes res)
                                                               repl
                                                               editor-state))
                  (callback))))

(defrecord ObjWithMore [obj-atom more-fn attributes-atom expanded? repl editor-state]
  proto/Renderable
  (as-text [_ ratom root?]
    (let [obj (assert-root (proto/as-text @obj-atom obj-atom root?))]
      (if expanded?
        (conj obj (proto/as-text @attributes-atom attributes-atom root?))
        (conj obj [:button "..." #(obj-with-more-fn more-fn ratom repl editor-state %)]))))

  (as-html [_ ratom root?]
    [:div {:class ["browseable"]}
     [:div {:class ["object"]}
      (proto/as-html @obj-atom obj-atom root?)
      (when more-fn
        [:a {:href "#"
             :on-click (fn [e]
                         (.preventDefault e)
                         (.stopPropagation e)
                         (obj-with-more-fn more-fn ratom repl editor-state identity))}
         (when root? "...")])]
     (when (and root? expanded?)
       [:div {:class "row children"}
        [proto/as-html @attributes-atom attributes-atom true]])]))

(declare ->indexed)
(defn- reset-atom [repl ratom obj result editor-state]
  (let [new-idx (->indexed result repl editor-state)]
    (swap! ratom
           (fn [indexed]
             (assoc indexed
                    :obj (vec (concat obj (:obj new-idx)))
                    :more-fn (:more-fn new-idx))))))

(defn- link-to-copy [ratom editor-state first-line-only?]
  [:a {:class "icon clipboard"
       :href "#"
       :on-click
       (fn [^js evt]
         (.preventDefault evt)
         (.stopPropagation evt)
         (copy-to-clipboard ratom editor-state first-line-only?))}])

(defrecord Indexed [open obj close kind expanded? more-fn repl editor-state]
  proto/Renderable
  (as-html [_ ratom root?]
    (let [a-for-more [:a {:href "#"
                          :on-click (fn [e]
                                      (.preventDefault e)
                                      (.stopPropagation e)
                                      (more-fn repl false #(reset-atom repl ratom obj
                                                                       % editor-state)))}
                      "..."]]

      [:div {:class ["row" kind]}
       [:div {:class ["coll" kind]}
        (when root?
          [:a {:class ["chevron" (if expanded? "opened" "closed")] :href "#"
               :on-click (fn [e]
                           (.preventDefault e)
                           (.stopPropagation e)
                           (swap! ratom update :expanded? not))}])
        [:div {:class "delim opening"} open]
        [:div {:class "inner"} (if (= "map" kind)
                                 (parse-inner-for-map obj more-fn a-for-more)
                                 (parse-inner-root obj more-fn a-for-more))]
        [:div {:class "delim closing"} close]
        (when root?
          [link-to-copy ratom editor-state true])]

       (when (and root? expanded?)
         [:div {:class "children"}
          [:<>
           (cond-> (mapv #(proto/as-html (deref %) % true) obj)
                   more-fn (conj a-for-more)
                   :then (->> (map (fn [i e] [:div {:key i :class "row"} e]) (range))))]])]))

  (as-text [_ ratom root?]
    (let [children (map #(proto/as-text @% % false) obj)
          toggle #(do (swap! ratom update :expanded? not) (%))
          extract-map #(-> % (textual->text false)
                           (str/replace #"^\[" "")
                           (str/replace #"\]$" ""))
          txt (if (= "map" kind)
                [:text (->> children
                            (map extract-map)
                            (str/join ", "))]
                [:text (->> children (map textual->text) (str/join " "))])
          more-callback (fn [callback]
                          (more-fn repl false
                                   #(do
                                      (reset-atom repl ratom obj % editor-state)
                                      (callback))))
          complete-txt (delay (if more-fn
                                (update txt 1 #(str open % " ..." close))
                                (update txt 1 #(str open % close))))
          root-part (delay [:expand (if expanded? "-" "+") toggle])
          rows (cond
                 (not root?) @complete-txt
                 more-fn [:row
                          @root-part
                          (update txt 1 #(str open % " "))
                          [:button "..." more-callback]
                          [:text close]]
                 :else [:row @root-part @complete-txt])]
      (if expanded?
        (cond-> (apply conj rows (map #(assert-root (proto/as-text @% % true)) obj))
                more-fn (conj [:row [:button "..." more-callback]]))
        rows))))

(defrecord Leaf [obj editor-state]
  proto/Renderable
  (as-html [_ ratom root?]
    (let [tp (cond
               (string? obj) "string"
               (number? obj) "number"
               (boolean? obj) "bool"
               (nil? obj) "nil"
               :else "other")]
      [:div {:class tp} (pr-str obj) (when root? [link-to-copy ratom editor-state true])]))
  (as-text [_ _ _]
    [:text (pr-str obj)]))

(defn- ->indexed [obj repl editor-state]
  (let [more-fn (eval/get-more-fn obj)
        children (mapv #(proto/as-renderable % repl editor-state) (eval/without-ellision obj))]
    (cond
      (vector? obj) (->Indexed "[" children "]" "vector" false more-fn repl editor-state)
      (set? obj) (->Indexed "#{" (vec children) "}" "set" false more-fn repl editor-state)
      (map? obj) (->Indexed "{" (vec children) "}" "map" false more-fn repl editor-state)
      (seq? obj) (->Indexed "(" children ")" "list" false more-fn repl editor-state))))

(defrecord IncompleteStr [string repl editor-state]
  proto/Renderable
  (as-html [_ ratom root?]
    [:div {:class "string big"}
     [:span (-> string eval/without-ellision pr-str (str/replace #"\"$" ""))]
     (when-let [get-more (eval/get-more-fn string)]
       [:a {:href "#"
            :on-click (fn [e]
                        (.preventDefault e)
                        (.stopPropagation e)
                        (get-more repl #(swap! ratom assoc :string %)))}
         "..."])
     "\""
     (when root? [link-to-copy ratom editor-state true])])

  (as-text [_ ratom root?]
    (if root?
      [:row
       [:text (-> string eval/without-ellision pr-str (str/replace #"\"$" ""))]
       [:button "..." #(let [f (eval/get-more-fn string)]
                         (f repl (fn [obj]
                                   (if (string? obj)
                                     (reset! ratom (->Leaf obj editor-state))
                                     (swap! ratom assoc :string obj))
                                   (%))))]
       [:text "\""]]
      [:text (pr-str string)])))

(defrecord Tagged [tag subelement editor-state open?]
  proto/Renderable
  (as-text [_ ratom root?]
    (let [toggle #(do (swap! ratom update :open? not) (%))]
      (if open?
        [:row [:expand "-" toggle]
         [:text tag] (proto/as-text @subelement subelement false)
         (assert-root (proto/as-text @subelement subelement true))]
        [:row [:expand "+" toggle] [:text tag] (proto/as-text @subelement subelement false)])))

  (as-html [_ ratom root?]
    (let [will-be-open? (and root? open?)
          copy-elem [link-to-copy ratom editor-state true]]
      [:div {:class "tagged"}
       (when root?
         [:a {:class ["chevron" (if open? "opened" "closed")] :href "#"
              :on-click (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          (swap! ratom update :open? not))}])
       [:div {:class [(when will-be-open? "row")]}
        [:div {:class "tag"} tag (when will-be-open? copy-elem)]
        [:div {:class [(when will-be-open? "tag children")]}
         [proto/as-html @subelement subelement will-be-open?]]
        (when (and (not open?) root?) copy-elem)]])))

(defrecord IncompleteObj [incomplete repl editor-state]
  proto/Renderable
  (as-text [_ ratom _]
    (let [more (eval/get-more-fn incomplete)]
      [:button "..." (fn [callback]
                       (more repl #(do
                                     (reset! ratom @(proto/as-renderable % repl editor-state))
                                     (callback))))]))

  (as-html [_ ratom _]
    (let [more (eval/get-more-fn incomplete)]
      [:div {:class "incomplete-obj"}
       [:a {:href "#" :on-click (fn [e]
                                  (.preventDefault e)
                                  (.stopPropagation e)
                                  (more repl #(reset! ratom @(proto/as-renderable % repl editor-state))))}
        "..."]])))

(defn- link-for-more-trace [repl ratom more-trace more-str callback?]
  (cond
    more-trace
    (fn [e]
      (when-not callback? (.preventDefault e) (.stopPropagation e))
      (more-trace repl #(do
                          (reset! ratom %)
                          (when callback? (e)))))

    more-str
    (fn [e]
      (when-not callback? (.preventDefault e) (.stopPropagation e))
      (more-str repl #(do
                        (swap! ratom assoc 2 %)
                        (when callback? (e)))))))

(defn- trace-span [file row]
  [:span {:class "file"} " (" file ":" row ")"])

(defn- trace-link [var file row editor-state cache-exists?]
  (let [{:keys [open-editor notify]} (:editor/callbacks @editor-state)
        {:keys [eql]} (:editor/features @editor-state)
        aux-repl (:clj/aux @editor-state)]
    [:a.file {:href "#"
              :on-click (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          (if cache-exists?
                            (open-editor {:file-name file :line (dec row)})
                            (p/let [exists? (cmds/run-callback! editor-state
                                                                :file-exists file)]
                              (if exists?
                                (open-editor {:file-name file :line (dec row)})
                                (def/goto-definition editor-state
                                  {:ex/function-name var
                                   :ex/filename file
                                   :ex/row row})))))}
     " (" file ":" row ")"]))

(defn- prepare-source-map [editor-state js-filename]
  (p/let [run-callback (:run-callback @editor-state)
          file-name (str js-filename ".map")
          contents (run-callback :read-file file-name)]
    (when contents
      (new SourceMapConsumer contents))))

(defn- resolve-source [^js sourcemap row col]
  (when-let [source (some-> sourcemap
                            (.originalPositionFor #js {:line (int row)
                                                       :column (int col)}))]
    (when (.-source ^js source)
      [(.-source ^js source) (.-line ^js source) (.-column ^js source)])))

(defn- demunge-js-name [js-name]
  (-> js-name
      (str/replace #"\$" ".")
      (str/replace #"(.*)\.(.*)$" "$1/$2")
      demunge))

(defn- trace-string [p-source idx ratom editor-state]
  (let [{:keys [open-editor notify]} (:editor/callbacks @editor-state)
        aux-repl (:clj/aux @editor-state)
        trace (get-in @ratom [:obj :trace idx])
        info (str/replace trace #"\(.*" "")
        filename-match (some-> (re-find #"\(.*\)" trace)
                               (str/replace #"[\(\)]" ""))
        local-row (r/atom (str/split filename-match #":"))
        loaded? (r/atom false)
        fun
        (fn []
          (let [[file row col] @local-row]
           (when-not @loaded?
             (p/let [[file row col] @local-row
                     file-contents (p-source file)
                     data (resolve-source file-contents row col)]
               (reset! loaded? true)
               (when data (reset! local-row data))))

           (if filename-match
             [:span.class (demunge-js-name info) "("
              [:a.file {:href "#"
                        :on-click (fn [e]
                                    (.preventDefault e)
                                    (.stopPropagation e)
                                    (p/let [exists? (cmds/run-callback! editor-state
                                                                        :file-exists file)]
                                      (if exists?
                                        (open-editor {:file-name file
                                                      :line (dec row)
                                                      :column (dec col)})
                                        ; FIXME - This may be possible now...
                                        ; (.. (definition/resolve-possible-path
                                        ;       aux-repl {:file file :line row})
                                        ;     (then #(open-editor (assoc %
                                        ;                                :line (dec row)
                                        ;                                :column (dec col))))
                                        (notify {:type :error
                                                 :title "Can't find file to go"}))))}

               file ":" row ":" col]
              ")"]
             [:span.stack-line trace])))]

    [:div {:key idx :class ["row" "clj-stack"]} [fun]]))

(defn- to-trace-row [p-source repl ratom editor-state idx trace]
  (let [[class method file row] trace
        link-for-more (link-for-more-trace repl
                                           (r/cursor ratom [:obj :trace idx])
                                           (eval/get-more-fn trace)
                                           (eval/get-more-fn file)
                                           false)
        clj-file? (re-find #"\.clj.?$" (str file))
        var (cond-> (str class) clj-file? demunge)]
    (cond
      (string? trace)
      (trace-string p-source idx ratom editor-state)

      link-for-more
      [:div {:key idx :class ["row" "incomplete"]}
       [:div "in " [:a {:href "#" :on-click link-for-more} "..."]]]

      (not= -1 row)
      [:div {:key idx :class ["row" (if clj-file? "clj-stack" "stack")]}
       [:div
        "in "
        (when var [:span {:class "class"} var])
        (when-not (or clj-file? (nil? method))
          [:span {:class "method"} "." method])
        (if clj-file?
          (trace-link var file row editor-state nil)
          (let [exists? (r/atom nil)
                res (fn []
                      (if @exists?
                        (trace-link var file row editor-state true)
                        (trace-span file row)))]
            (.then (cmds/run-callback! editor-state :file-exists (str file))
                   #(reset! exists? %))
            [res]))]])))
          ; :else (trace-span file row))]])))

(defn- to-trace-row-txt [repl ratom idx trace]
  (let [[class method file row] trace
        link-for-more (link-for-more-trace repl
                                           (r/cursor ratom [:obj :trace idx])
                                           (eval/get-more-fn trace)
                                           (eval/get-more-fn file)
                                           true)
        clj-file? (re-find #"\.clj?$" (str file))]
    (cond
      (string? trace) [:row [:text trace]]

      link-for-more [:row [:text "in "] [:button "..." link-for-more]]

      (not= -1 row)
      [:row
       [:text
        (str "in " (cond-> (str class) clj-file? demunge)
             (when-not clj-file? (str "." method))
             " (" file ":" row ")")]])))

(defrecord ExceptionObj [obj add-data repl editor-state]
  proto/Renderable
  (as-text [_ ratom root?]
    (let [{:keys [type message trace]} obj
          ex (proto/as-text @message message true)
          ex (if (-> ex first (= :row))
                (update-in ex [1 1] #(str type ": " %))
                [:row (update ex 1 #(str type ": " %))])

          traces (map (partial to-trace-row-txt repl ratom)
                      (range)
                      (eval/without-ellision trace))]
      (if add-data
        (apply conj ex (proto/as-text @add-data add-data root?) traces)
        (apply conj ex traces))))

  (as-html [_ ratom root?]
    (let [{:keys [type message trace]} obj
          p-source (memoize #(prepare-source-map editor-state %))]
      [:div {:class "exception row"}
       [:div {:class "description"}
        [:span {:class "ex-kind"} (str type)] ": " [proto/as-html @message message root?]]
       (when (and add-data root?)
         [:div {:class "children additional"}
          [proto/as-html @add-data add-data root?]])
       (when root?
         [:div {:class "children"}
          (doall
            (map (partial to-trace-row p-source repl ratom editor-state)
                 (range)
                 (eval/without-ellision trace)))
          (when-let [more (eval/get-more-fn trace)]
            [:a {:href "#" :on-click (fn [e]
                                       (.preventDefault e)
                                       (.stopPropagation e)
                                       (more repl #(swap! ratom assoc-in [:obj :trace] %)))}
             "..."])])])))

(defrecord Patchable [id value]
  proto/Renderable
  (as-text [_ ratom root?] (proto/as-text @value value root?))
  (as-html [self ratom root?]
    [proto/as-html @(:value @ratom) (:value @ratom) root?]))

(extend-protocol proto/Parseable
  helpers/Interactive
  (as-renderable [self repl editor-state]
    (r/atom (int/->Interactive (.-edn self) repl editor-state)))

  helpers/Error
  (as-renderable [self repl editor-state]
    (let [obj (update self :message proto/as-renderable repl editor-state)
          add-data (some-> self :add-data not-empty (proto/as-renderable repl editor-state))]
      (r/atom (->ExceptionObj obj add-data repl editor-state))))

  helpers/IncompleteObj
  (as-renderable [self repl editor-state]
    (r/atom (->IncompleteObj self repl editor-state)))

  helpers/IncompleteStr
  (as-renderable [self repl editor-state]
    (r/atom (->IncompleteStr self repl editor-state)))

  helpers/Browseable
  (as-renderable [self repl editor-state]
    (let [{:keys [object attributes]} self]
      (r/atom (->ObjWithMore (proto/as-renderable object repl editor-state)
                             (eval/get-more-fn self)
                             (proto/as-renderable attributes repl editor-state)
                             false
                             repl
                             editor-state))))

  helpers/WithTag
  (as-renderable [self repl editor-state]
    (let [tag (helpers/tag self)
          subelement (-> self helpers/obj (proto/as-renderable repl editor-state))]
      (r/atom (->Tagged tag subelement editor-state false))))

  helpers/LiteralRender
  (as-renderable [obj repl editor-state]
    (r/atom (->Leaf obj editor-state)))

  helpers/Patchable
  (as-renderable [{:keys [id value]} repl editor-state]
    (r/atom (->Patchable id (proto/as-renderable value repl editor-state))))

  default
  (as-renderable [obj repl editor-state]
    (r/atom
      (cond
        (coll? obj) (->indexed obj repl editor-state)
        :else (->Leaf obj editor-state)))))

(defn parse-result
  "Will parse a result that comes from the REPL in a r/atom so that
it'll be suitable to be rendered with `view-for-result`"
  [result repl editor-state]
  (let [parsed (helpers/parse-result result)]
    (if (contains? parsed :result)
      (proto/as-renderable (:result parsed) repl editor-state)
      (let [error (:error parsed)
            ; FIXME: is this really necessary? Can we use the exception renderer?
            ex (cond-> error
                       (:ex error) :ex
                       (->> error :ex (instance? helpers/Browseable)) :object)]
        (with-meta (proto/as-renderable ex repl editor-state) {:error true})))))

(defn view-for-result
  "Renders a view for a result. If it's an error, will return a view
suitable for error backtraces. If it's a success, will return a success
view. Expects a r/atom that comes from `parse-result`"
  [state]
  [proto/as-html @state state true])

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
  (assert-root (proto/as-text @state state true)))

(defn- parse-funs [funs last-elem curr-text elem]
  (let [txt-size (-> elem (nth 1) count)
        curr-row (count curr-text)
        fun (peek elem)]
    (reduce (fn [funs col] (assoc funs [last-elem col] fun))
            funs (range curr-row (+ curr-row txt-size)))))

(defn- parse-elem [position lines funs depth]
  (let [[elem text] position
        last-elem (-> lines count dec)
        indent (->> depth (* 2) range (map (constantly " ")) (apply str) delay)
        last-line (peek lines)
        curr-text (if (empty? last-line)
                    @indent
                    last-line)]
    (case elem
      :row (recur (rest position) (conj lines "") funs (inc depth))
      :text [(assoc lines last-elem (str curr-text text)) funs]
      :button [(assoc lines last-elem (str curr-text text))
               (parse-funs funs last-elem curr-text position)]
      :expand [(assoc lines last-elem (str curr-text text "  "))
               (parse-funs funs last-elem curr-text position)]
      (reduce (fn [[lines funs] position] (parse-elem position lines funs depth))
              [lines funs] position))))

(defn repr->lines [repr]
  (parse-elem repr [] {} -1))
