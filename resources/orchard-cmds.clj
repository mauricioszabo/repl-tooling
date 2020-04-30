(defn info [ns-name var-name params]
  (let [s (orchard.info/info (symbol ns-name) (symbol var-name) (eval params))]
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
      :fns {:info (list 'fn '[_ s var-name]
                        (list 'orchard.info/info '(symbol (namespace var-name))
                                                 '(symbol (name var-name))
                              params))
            :change-class '(fn [e s]
                             (assoc s :sel (symbol (:value e))))}})))
