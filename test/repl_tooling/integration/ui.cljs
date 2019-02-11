(ns repl-tooling.integration.ui
  (:require [reagent.core :as r]
            [clojure.test :refer [async testing is] :include-macros true]
            [check.core :refer-macros [check]]
            [check.async :include-macros true]
            [clojure.core.async :as async :include-macros true]
            [devcards.core :as cards :include-macros true]
            [clojure.string :as str]
            [repl-tooling.editor-helpers-test]

            [repl-tooling.editor-integration.connection :as conn]))

(defonce state (r/atom {:host "localhost"
                        :port 2233
                        :code "(+ 1 2)"
                        :repls {:eval nil
                                :aux nil}
                        :commands {}
                        :stdout nil
                        :stderr nil
                        :eval-result nil}))

(defn disconnect! []
  (conn/disconnect!))

(defn handle-disconnect []
  (swap! state assoc
         :repls {:eval nil :aux nil}
         :stdout nil :stderr nil :eval-result nil
         :commands {}))

(defn connect! []
  (when-not (-> @state :repls :eval)
    (.
      (conn/connect! (:host @state) (:port @state)
                     {:on-disconnect handle-disconnect
                      :on-stdout #(swap! state update :stdout (fn [e] (str e %)))
                      :on-eval #(swap! state update :stdout (fn [e] (str e "=> " (:as-text %) "\n")))
                      :on-stderr #(swap! state update :stderr (fn [e] (str e %)))
                      :editor-data #(let [code (:code @state)
                                          lines (str/split-lines code)]
                                      {:contents code
                                       :filename "foo.clj"
                                       :range [[0 0]
                                               [(-> lines count dec) (-> lines last count dec)]]})})
      (then (fn [res]
              (swap! state assoc :repls {:eval (:clj/repl res)
                                         :aux (:clj/aux res)}
                     :commands (:editor/commands res)
                     :stdout "" :stderr ""))))))

(defn- evaluate []
  ((-> @state :commands :evaluate-selection :command)))

(defn fake-editor [state]
  [:div
   [:h4 "Socket REPL connections"]
   [:p [:b "Hostname: "] [:input {:type "text" :value (:host @state)
                                  :on-change #(->> % .-target .-value (swap! state assoc :host))}]
       [:b " Port: "] [:input {:type "text" :value (:port @state)
                               :on-change #(->> % .-target .-value int (swap! state assoc :port))}]]
   [:textarea {:style {:width "100%" :height "200px"}
               :value (:code @state)
               :on-change #(->> % .-target .-value (swap! state assoc :code))}]
   [:p
    (if (-> @state :repls :eval)
      [:span
       [:button {:on-click evaluate}
        "Evaluate"] " "
       [:button {:on-click disconnect!} "Disconnect!"]]
      [:button {:on-click connect!} "Connect!"])]
   [:p (if (-> @state :repls :eval) "Connected" "Disconnected")]
   (when-let [out (:stdout @state)]
     [:div
      [:h5 "STDOUT"]
      [:pre out]])
   (when-let [out (:stderr @state)]
     [:div
      [:h5 "STDERR"]
      [:pre out]])])

(cards/defcard-rg fake-editor
  fake-editor
  state
  {:inspect-data true})

(defn wait-for [f]
  (async/go
   (loop [t 0]
     (when (< t 100)
       (if-let [res (f)]
         res
         (do
           (async/<! (async/timeout 100))
           (recur (inc t))))))))

(defn- type-in [txt] (swap! state assoc :code txt))
(defn- type-and-eval [txt]
  (swap! state assoc :code txt)
  (evaluate))
(defn- txt-in-stdout [reg]
  (wait-for #(re-find reg (:stdout @state))))
(defn- change-stdout []
  (let [old (:stdout @state)]
    (wait-for #(and (not= old (:stdout @state))
                    (:stdout @state)))))
(defn- change-stderr []
  (let [old (:stderr @state)]
    (wait-for #(and (not= old (:stderr @state))
                    (:stderr @state)))))

(set! cards/test-timeout 8000)
(cards/deftest repl-evaluation
  (async done
    (async/go
     (connect!)
     (async/<! (wait-for #(-> @state :repls :eval)))

     (testing "evaluation works"
       (type-and-eval "(+ 2 3)")
       (check (async/<! (txt-in-stdout #"=> 5")) => "=> 5"))

     (testing "captures STDOUT"
       (type-and-eval "(println :FOOBAR)")
       (check (async/<! (change-stdout)) => #":FOOBAR"))

     (testing "captures STDERR"
       (type-and-eval "(.write *err* \"Error\")")
       (check (async/<! (change-stderr)) => #"Error"))

     (testing "detects NS on file"
       (type-and-eval "(do (ns clojure.string)\n(upper-case \"this is upper\"))")
       (check (async/<! (change-stdout)) => #"THIS IS UPPER"))

     (disconnect!)
     (done))))

(defn main [])
(cards/start-devcard-ui!)
