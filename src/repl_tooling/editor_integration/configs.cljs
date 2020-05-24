(ns repl-tooling.editor-integration.configs
  (:require [sci.core :as sci]
            [clojure.set :as set]
            [promesa.core :as p]
            [paprika.collection :as coll]
            [clojure.string :as str]
            [repl-tooling.editor-integration.commands :as cmds]
            [sci.impl.namespaces :as sci-ns]
            [pinkgorilla.ui.pinkie :as pinkie]
            [pinkgorilla.ui.jsrender :as jsrender]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            ["path" :refer [dirname join]]
            ["fs" :refer [watch readFile existsSync]]))

(defn- read-config-file [config-file]
  (let [p (p/deferred)]
    (readFile config-file #(p/resolve! p (str %2)))
    p))

(defn- name-for [k]
  (-> k name str/capitalize (str/replace #"-" " ")))

(def ^:private promised-let
  ^:sci/macro
  (fn [_&form _&env bindings & body]
    (let [binds (->> bindings (partition-all 2 2) reverse)]
      (loop [body (cons 'do body)
             [[var elem] & rest] binds]
        (if (nil? var)
          body
          (recur
            (list 'then (list 'promise elem) (list 'fn [var] body))
            rest))))))

(defn- find-repl [state]
  (p/let [data (cmds/run-callback! state :editor-data)]
    (cmds/run-feature! state :repl-for (:filename data) true)))

(defn- editor-ns [repl state]
  (let [repl (delay (or repl (find-repl state)))]
    {'run-callback (partial cmds/run-callback! state)
     'run-feature (fn [cmd & args]
                    (p/let [curr-repl @repl]
                      (if (= cmd :go-to-var-definition)
                        (cmds/run-feature! state
                                           :go-to-var-definition
                                           (assoc (first args)
                                                  :repl curr-repl))
                        (apply cmds/run-feature! state cmd args))))
     'get-top-block #(cmds/run-feature! state :get-code :top-block)
     'get-block #(cmds/run-feature! state :get-code :block)
     'get-var #(cmds/run-feature! state :get-code :var)
     'get-selection #(cmds/run-feature! state :get-code :selection)
     'get-namespace #(p/let [res (cmds/run-feature! state :get-code :ns)]
                       (update res :text str))
     'eval-and-render #(cmds/run-feature! state :evaluate-and-render %)
     'eval-interactive #(cmds/run-feature! state :evaluate-and-render
                                           (update % :pass assoc
                                                   :interactive true
                                                   :aux true))
     'eval (partial cmds/run-feature! state :eval)}))

(defn- norm-reagent-fn [fun]
  (fn [ & args]
    (let [state (r/atom [:div.repl-tooling.icon.loading])]
      (apply fun (js->clj args))
      [:<> @state])))

(defn- norm-pinkie-fn [fun]
  (fn [ & args]
    [jsrender/render-js
     {:f (fn [dom args]
           (let [div (.createElement js/document "div")]
             (p/let [elem (apply fun (js->clj args))] (.replaceChild dom elem div))
             (.. div -classList (add "repl-tooling" "icon" "loading"))
             (.appendChild dom div)))
      :data args}]))

(defn- render-ns [editor-state]
  (def es editor-state)
  {'js-require #(-> @editor-state
                    :editor/callbacks
                    :config-file-path
                    dirname
                    (join %)
                    js/require)
   'create-tag #(.createElement js/document %)
   'set-text #(aset %1 "innerText" %2)
   'set-html #(aset %1 "innerHTML" %2)
   ; 'register-reagent #(if (and (keyword? %1) (namespace %1) (fn? %2))
   ;                      (pinkie/register-tag %1 (norm-reagent-fn %2))
   ;                      (cmds/run-callback!
   ;                       editor-state
   ;                       :notify
   ;                       {:type :error
   ;                        :title "Invalid params"
   ;                        :text (str "First argument needs to be a namespaced keyword, "
   ;                                   "and second argument needs to be a reagent fn")}))
   'register-tag #(if (and (keyword? %1) (namespace %1) (fn? %2))
                    (pinkie/register-tag %1 (norm-pinkie-fn %2))
                    (cmds/run-callback!
                     editor-state
                     :notify
                     {:type :error
                      :title "Invalid params"
                      :text (str "First argument needs to be a namespaced keyword, "
                                 "and second argument needs to be a function that "
                                 "returns a HTML tag")}))})

(defn- prepare-nses [repl editor-state]
  (-> sci-ns/namespaces
      (set/rename-keys '{clojure.string str
                         clojure.set set
                         clojure.walk walk
                         clojure.template template
                         clojure.repl repl
                         clojure.edn edn})
      (assoc 'r {'atom r/atom
                 'adapt-react-class r/adapt-react-class
                 'as-element r/as-element
                 'create-class r/create-class
                 'create-element r/create-element
                 'current-component r/current-component
                 'cursor r/cursor
                 'is-client r/is-client
                 'reactify-component r/reactify-component
                 'wrap r/wrap})
      (assoc 'render (render-ns editor-state))
      (assoc 'editor (editor-ns repl editor-state))))

(def ^:private promised-bindings {'promise #(.resolve js/Promise %)
                                  'then #(.then ^js %1 %2)
                                  'catch #(.catch ^js %1 %2)
                                  'let promised-let})

(defn default-bindings [editor-state]
  (assoc promised-bindings
         'println (fn [& args]
                    (cmds/run-callback! editor-state :on-stdout
                                        (str (str/join " " args) "\n")))
         'print (fn [& args]
                   (cmds/run-callback! editor-state :on-stdout
                                       (str/join " " args)))
         'prn (fn [& args]
                (->> args (map pr-str)
                     (str/join " ")
                     (#(str % "\n"))
                     (cmds/run-callback! editor-state :on-stdout)))
         'pr (fn [& args]
                (->> args (map pr-str)
                     (str/join " ")
                     (cmds/run-callback! editor-state :on-stdout)))))

(defn evaluate-code [{:keys [code bindings sci-state editor-state repl]
                      :or {bindings promised-bindings
                           sci-state (atom {})}}]
  (sci/eval-string code {:env sci-state
                         :preset {:termination-safe true}
                         :namespaces (prepare-nses repl editor-state)
                         :bindings bindings}))

(defn- catch-errors [fun editor-state]
  (try
    (fun)
    (catch :default e
      (cmds/run-callback! editor-state :on-eval
                          {:id (gensym "custom-eval")
                           :editor-data {:contents "[INTERNAL-FN]"
                                         :range [[0 0] [0 0]]
                                         :filename (-> @editor-state
                                                       :editor/callbacks
                                                       :config-file-path)}
                           :range [[0 0] [0 0]]
                           :repl nil
                           :result {:error e
                                    :as-text (pr-str e)
                                    :parsed? true}}))))

(defn- fns-for [editor-state config-file]
  (when (existsSync config-file)
    (p/let [config (read-config-file config-file)
            sci-state (atom {})
            bindings (default-bindings editor-state)
            _ (evaluate-code {:code config
                              :bindings bindings
                              :sci-state sci-state
                              :editor-state editor-state})
            vars (->> (sci/eval-string "(->> *ns* ns-publics keys)" {:env sci-state})
                      (map #(vector % (sci/eval-string (str %) {:env sci-state}))))]
      (->> vars
           (filter (fn [[k v]] (and (fn? v) (not (contains? bindings k)))))
           (reduce (fn [acc [k fun]]
                     (assoc acc (-> k str keyword) {:name (name-for k)
                                                    :command #(catch-errors
                                                               fun editor-state)}))
                   {})))))

(declare reg-commands)
(defn- watch-config [editor-state cmds-from-tooling config-file]
  (when config-file
    (let [dir (dirname config-file)
          watch-pid (watch dir
                           (fn [evt filename]
                             (when (= (join dir filename) config-file)
                               (reg-commands editor-state cmds-from-tooling config-file))))
          old-disconnect (-> @editor-state :editor/callbacks :on-disconnect)]
      (swap! editor-state assoc-in
             [:editor/callbacks :on-disconnect] (fn []
                                                   (old-disconnect)
                                                   (.close ^js watch-pid))))))

(defn- reg-commands [editor-state cmds-from-tooling config-file]
  (p/let [cmds-from-config (fns-for editor-state config-file)
          commands (merge cmds-from-tooling cmds-from-config)]
    (swap! editor-state assoc :editor/commands commands)
    (cmds/run-callback! editor-state :register-commands commands)))

(defn prepare-commands [editor-state cmds-from-tooling]
  (swap! editor-state assoc :editor/commands cmds-from-tooling)
  (p/let [config-file (-> @editor-state :editor/callbacks :config-file-path)]
    (watch-config editor-state cmds-from-tooling config-file)
    (reg-commands editor-state cmds-from-tooling config-file)))
