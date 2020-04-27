(defn info! [ns-name var-name])

#_
(let [ns-name "user"
      var-name "prn"
      ?state (orchard.info/info (symbol ns-name) (symbol var-name))
      ?state (assoc ?state :doc "lol\nima\ndoc")]
  (tagged-literal
   'repl-tooling/interactive
   {:html '[:div.rows
            [:div.title (str (:ns ?state) "/" (str (:name ?state)))]
            [:div.indent (pr-str (:arglists ?state))]
            [:div.space]
            [:div.pre (:doc ?state)]
            [:div.space]
            [:div.rows
             [:div [:i "See also:"]]
             [:<>
              (map (fn [e i] [:div {:key i } [:a {:href "#"
                                                  :on-click (fn [])}
                                              e]])
                   (:see-also ?state) (range))]]]
    :state ?state}))

(let [ns-name "user"
      var-name "prn"
      s (-> (orchard.info/info (symbol ns-name) (symbol var-name))
            (update :arglists pr-str)
            (update :see-also #(map (fn [e i] [:div {:key i} [:a {:href "#"
                                                                  :on-click '?info}
                                                              e]])
                                    % (range)))
            (assoc :doc "lol\nima\ndoc"))]
  (tagged-literal
   'repl-tooling/interactive
   {:html '[:div.rows
            [:div.title (:ns ?state) "/" (:name ?state)]
            [:div.indent (:arglists ?state)]
            [:div.space]
            [:div.pre (:doc ?state)]
            [:div.space]
            [:div.rows
             [:div [:i "See also:"]]
             [:<> (:see-also ?state)]]]
    :state s
    :fns {:info (fn [_ _] (prn :STATE))}}))
