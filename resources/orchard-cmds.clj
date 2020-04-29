(defn info [ns-name var-name params]
  (let [s (orchard.info/info (symbol ns-name) (symbol var-name) params)]
    (tagged-literal
     'repl-tooling/interactive
     {:html '(cond
               (nil? ?state)
               [:div.title "Nothing found for this var"]

               :else
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
                           (:see-also ?state) (range))]]])
      :state s
      :fns {:info (list 'fn '[_ s var-name]
                        (list 'orchard.info/info '(symbol (namespace var-name))
                                                 '(symbol (name var-name))
                              params))}})))
