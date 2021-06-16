(ns repl-tooling.commands-to-repl.pathom
  (:require [promesa.core :as p]
            [duck-repled.core :as duck]
            [duck-repled.repl-protocol :as duck-repl]
            [repl-tooling.eval :as eval]
            [com.wsscode.pathom3.connect.operation :as connect]))

(defonce ^:private global-eql (atom nil))
(defonce ^:private global-resolvers (atom nil))
(defonce ^:private orig-resolvers (atom nil))

(defn reset-resolvers []
  (reset! global-resolvers @orig-resolvers)
  (reset! global-eql (duck/gen-eql {:resolvers @orig-resolvers})))

(defn add-resolver [config fun]
  (let [old @global-resolvers
        new (duck/add-resolver old config fun)]
    (reset! global-resolvers new)
    (reset! global-eql (duck/gen-eql {:resolvers new}))))

(defn compose-resolver [config fun]
  (let [old @global-resolvers
        new (duck/compose-resolver old config fun)]
    (reset! global-resolvers new)
    (reset! global-eql (duck/gen-eql {:resolvers new}))))

#_@global-resolvers

(defrecord REPL [evaluator]
  duck-repl/Evaluator
  (-evaluate [_ command options]
    (eval/eval evaluator command options)))

(defn- adapt-repl [evaluator]
  (if evaluator
    (->REPL evaluator)
    :com.wsscode.pathom3.connect.operation/unknown-value))

(defn- resolvers-from-state [editor-state]
  (p/let [{:keys [editor/callbacks]} @editor-state
          editor-data ((:editor-data callbacks))
          config ((:get-config callbacks))
          not-found :com.wsscode.pathom3.connect.operation/unknown-value]
    {:editor/data (or editor-data not-found)
     :config/eval-as (:eval-mode config)
     :config/project-paths (vec (:project-paths config))
     ; FIXME: Get the right REPL
     :repl/evaluators {:clj (adapt-repl (:clj/aux @editor-state))
                       :cljs (adapt-repl (:cljs/repl @editor-state))}
     :config/repl-kind (-> @editor-state :repl/info :kind)}))

(def ^:private doc-part
  '(when-let [text (:doc ?state)]
     [:<>
      [:div.space]
      (if (:markdown? ?state)
        [:div/md text]
        [:div.pre text])]))

(def ^:private spec-part
  '(when-let [spec (:spec ?state)]
     [:<>
      [:div.space]
      [:div.pre
       (cond-> "Spec:\n"
               (:args spec) (str "  args: " (pr-str (:args spec)) "\n")
               (:ret spec) (str "  ret: " (pr-str (:ret spec)) "\n")
               (:fn spec) (str "  fn: " (pr-str (:fn spec))))]]))

(def ^:private markdown-check
  '(when (:doc ?state)
     [:<>
      [:div.space]
      [:label [:input {:type :checkbox
                       :checked (:markdown? ?state)
                       :on-click (fn [e]
                                   (swap! ?state-atom update :markdown? not))}]
       " Use markdown"]]))

(def ^:private var-contents
  '(if (empty? (:arglists ?state))
     [:div.rows
      [:div.space]
      (if (contains? ?state :var-value)
        [:div/clj (:var-value ?state)]
        [:div [:a {:href "#"
                   :on-click (?get-contents (->> ?state :fqn eval))}
               "Get contents of var"]])]
     [:div.rows
      [:div.space]
      (if (contains? ?state :var-value)
        [:div/md (str "```\n" (:var-value ?state) "\n```")]
        [:div [:a {:href "#"
                   :on-click (fn [evt]
                               (.preventDefault evt)
                               (.stopPropagation evt)
                               (p/let [info (eql [{:editor/contents
                                                   [{:text/current-var [:definition/source]}]}])]
                                 (swap! ?state-atom
                                        assoc
                                        :var-value
                                        (-> info
                                            :editor/contents
                                            :text/current-var
                                            :definition/source
                                            :text/contents))))}
               "Get source"]])]))

(defn- improved-doc-for-var [{:var/keys [fqn meta spec]}]
  {:render/doc
   {:html [:div.rows
           '[:div.title (-> ?state :fqn str)]
           '(when-let [args (seq (:arglists ?state))]
              (map (fn [a] [:li {:key a} (pr-str a)]) args))

           '(cond-> [:div.cols]
                    (:macro ?state) (conj [:i "macro"])
                    (:private ?state) (conj [:i "private"]))
           doc-part
           spec-part
           markdown-check
           var-contents]
    :state (-> meta
               (dissoc :ns)
               (assoc :markdown? true :fqn fqn :spec spec))
    :fns {:get-contents '(fn [_ state value]
                           (assoc state :var-value value))}}})

(defn eql-from-state [editor-state]
  (let [resolver #(resolvers-from-state editor-state)
        resolvers (duck/add-resolver {:inputs []
                                      :outputs [:editor/data :config/eval-as
                                                :config/project-paths :config/repl-kind]}
                                     resolver)
        resolvers (duck/add-resolver resolvers
                                     {:inputs [:var/fqn :var/meta (connect/? :var/spec)]
                                      :outputs [:render/doc]}
                                     improved-doc-for-var)]
    (reset! orig-resolvers resolvers)
    (reset! global-resolvers resolvers)
    (reset! global-eql (duck/gen-eql {:resolvers resolvers}))
    (fn eql
      ([query] (@global-eql query))
      ([seed query] (@global-eql (or seed {}) query)))))
