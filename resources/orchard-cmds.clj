(defn info! [ns-name var-name]
  (let [s (orchard.info/info (symbol ns-name) (symbol var-name))]
    (tagged-literal
     'repl-tooling/interactive
     {:html '[:div.rows
              [:div.title (str (:ns ?state) "/" (:name ?state))]
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
      :state s
      :fns {:info '(fn [_ s var-name]
                     (orchard.info/info (symbol (namespace var-name))
                                        (symbol (name var-name))))}})))
