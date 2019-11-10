(ns repl-tooling.integration.fake-editor
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [repl-tooling.editor-integration.renderer :as render]
            [repl-tooling.editor-integration.connection :as conn]))

(defonce state (r/atom {:host "localhost"
                        :port 2233
                        :code "(do (defrecord Foo [a b]) (->Foo (range 20) 20))"
                        :repls {:eval nil
                                :aux nil}
                        :commands {}
                        :stdout nil
                        :stderr nil
                        :range [[0 0]]
                        :eval-result (r/atom nil)}))

(defn- res [{:keys [result]}]
  (reset! (:eval-result @state) (render/parse-result result (-> @state :repls :eval)))
  (swap! state update :stdout (fn [e] (str e "=> " (:as-text result) "\n"))))

(defn evaluate []
  (let [lines (-> @state :code str/split-lines)
        eval-sel (-> @state :commands :evaluate-selection :command)]
    (swap! state assoc :range [[0 0] [(-> lines count dec) (-> lines last count dec)]])
    (eval-sel)))

(defn handle-disconnect []
  (reset! (:eval-result @state) nil)
  (swap! state assoc
         :repls {:eval nil :aux nil}
         :stdout nil :stderr nil
         :commands {}))

(defn connect!
  ([] (connect! {}))
  ([additional-callbacks]
   (when-not (-> @state :repls :eval)
     (.
       (conn/connect! (:host @state) (:port @state)
                      (merge {:on-disconnect handle-disconnect
                              :on-stdout #(swap! state update :stdout (fn [e] (str e %)))
                              :on-eval res
                              :notify identity
                              :on-stderr #(swap! state update :stderr (fn [e] (str e %)))
                              :editor-data #(let [code (:code @state)]
                                              {:contents code
                                               :filename "foo.clj"
                                               :range (:range @state)})}
                             additional-callbacks))
       (then (fn [res]
               (swap! state assoc :repls {:eval (:clj/repl @res)
                                          :aux (:clj/aux @res)}
                      :commands (:editor/commands @res)
                      :stdout "" :stderr "")))))))

(defn editor [state]
  [:div
   [:h4 "Socket REPL connections"]
   [:p [:b "Hostname: "] [:input {:type "text" :value (:host @state)
                                  :on-change #(->> % .-target .-value (swap! state assoc :host))}]
       [:b " Port: "] [:input {:type "text" :value (:port @state)
                               :on-change #(->> % .-target .-value int (swap! state assoc :port))}]]
   [:textarea {:style {:width "100%" :height "100px"}
               :value (:code @state)
               :on-change #(->> % .-target .-value (swap! state assoc :code))}]
   [:p
    (if (-> @state :repls :eval)
      [:span
       [:button {:on-click evaluate}
        "Evaluate"] " "
       [:button {:on-click conn/disconnect!} "Disconnect!"]]
      [:button {:on-click connect!} "Connect!"])]
   [:p (if (-> @state :repls :eval) "Connected" "Disconnected")]
   [:div
    (when-let [res @(:eval-result @state)]
      [:div
       [:h5 "RESULT"]
       [:pre
        [:div {:id "result" :class "result"}
         (render/view-for-result res)]]])]
   (when-let [out (:stdout @state)]
     [:div
      [:h5 "STDOUT"]
      [:pre out]])
   (when-let [out (:stderr @state)]
     [:div
      [:h5 "STDERR"]
      [:pre out]])])
