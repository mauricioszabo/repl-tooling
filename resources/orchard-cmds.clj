(defn info [ns-name var-name params]
  (clojure.core/let [s (orchard.info/info (symbol ns-name) (symbol var-name) (eval params))]
    (tagged-literal
     'repl-tooling/interactive
     {:html '(cond
               (:ns ?state)
               [:div.rows
                [:div.title (cond->> (str (:name ?state))
                                     (:ns ?state) (str (:ns ?state) "/"))]
                [:div.indent (str (:arglists ?state))]
                [:div.space]
                [:div.pre (str (:doc ?state))]
                [:div.space]
                [:div.rows
                 [:div [:i "See also:"]]
                 [:<> (map (fn [e i] [:div {:key i}
                                      [:a {:href "#"
                                           :on-click (?info e)}
                                       (pr-str e)]])
                           (:see-also ?state) (range))]]]

               (:candidates ?state)
               [:div.rows
                [:select {:on-change ?change-class}
                 (->> ?state :candidates
                      (sort-by first)
                      (map (fn [[class cand]]
                             [:option {:key class :value (str class)}
                              (str class)])))]
                (let [sel (or (:sel ?state) (-> ?state :candidates keys sort first))
                      sel (get-in ?state [:candidates sel])]
                  [:<>
                   [:div (pr-str (:sel ?state))]
                   [:div.title (str (str/join (:modifiers sel) " ")
                                    " " (:class sel) "#" (:member sel))]
                   [:div.indent (str (:arglists sel))]
                   [:div.space]
                   [:div.pre (str (:doc sel))]
                   [:div.pre (str "=> " (:returns sel))]
                   [:div.space]])]

               :else
               [:div.title "Nothing found for this var"])

      :state s
      :fns {:info (clojure.core/list 'fn '[_ s var-name]
                        (clojure.core/list 'orchard.info/info '(symbol (namespace var-name))
                                                              '(symbol (name var-name))
                                           params))
            :change-class '(fn [e s]
                             (assoc s :sel (symbol (:value e))))}})))

(require '[orchard.xref])
(defn find-usages [symbol-name]
  (let [sym (symbol symbol-name)
        refs (orchard.xref/fn-refs sym)
        grouped (clojure.core/group-by #(-> % meta :ns str) refs)]
    {:html
     `[:div.rows
       [:div.title "Occurrences of " ~symbol-name ":"]
       [:div.space]
       ~@(for [ns-name (sort (keys grouped))]
           `[:div.rows
             [:div.title "In namespace: " ~ns-name]
             ~@(for [variable (get grouped ns-name)
                     :let [v (str (symbol variable))]]
                 [:div [:a {:href "#"
                            :on-click (list 'fn '[_]
                                            (list 'editor/run-feature
                                                  :go-to-var-definition
                                                  {:namespace "user"
                                                   :var-name v}))}
                        v]])
             [:div.space]])]}))

(require '[orchard.clojuredocs])
(defn clojure-docs [ns-name var-name]
  (let [doc
        (orchard.clojuredocs/find-doc ns-name var-name)]
    {:html '(let [{:keys [doc nodes examples see-alsos ns name arglists]} ?state
                  fqn (str ns "/" name)]
              [:div.rows
               [:div.title fqn]
               [:<> (map (fn [a] [:div {:key a} "(" fqn " " a ")"]) arglists)]
               [:div.space]
               [:div.pre doc]
               [:div.space]
               [:div.title (count examples) " example(s)"]
               [:<>
                (map (fn [ex i]
                       (if ((:pages ?state) i)
                         [:div.rows {:key i}
                          [:div.cols
                           [:a.chevron.opened {:href "#" :on-click (?close i)}]
                           [:div.space]
                           [:a.icon.clipboard
                                       {:on-click (fn [_] (editor/run-callback :on-copy ex))}]
                           [:div.pre ex]]
                          [:div.space]]
                         [:div.rows {:key i}
                          [:div.cols
                           [:a.chevron.closed {:href "#" :on-click (?open i)}]
                           [:div.space]
                           (->> ex (take 10) (apply str)) "..."]
                          [:div.space]]))
                     examples (range))]])
     :state (assoc doc :pages #{0})
     :fns '{:open (fn [_ s idx] (update s :pages conj idx))
            :close (fn [_ s idx] (update s :pages disj idx))}}))
