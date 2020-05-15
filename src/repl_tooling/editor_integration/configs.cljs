(ns repl-tooling.editor-integration.configs
  (:require [sci.core :as sci]
            [promesa.core :as p]
            [paprika.collection :as coll]
            [clojure.string :as str]
            [repl-tooling.editor-integration.commands :as cmds]
            [sci.impl.namespaces :as sci-ns]
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
     'get-namespace #(cmds/run-feature! state :get-code :ns)
     'eval-and-render #(cmds/run-feature! state :evaluate-and-render %)
     'eval (partial cmds/run-feature! state :eval)}))

(defn- prepare-nses [repl editor-state]
  (assoc sci-ns/namespaces
         'editor (editor-ns nil editor-state)))

(defn evaluate-code [code sci-state editor-state]
  (sci/eval-string code {:env sci-state
                         :namespaces (prepare-nses nil editor-state)
                         :bindings {'promise #(.resolve js/Promise %)
                                    'then #(.then ^js %1 %2)
                                    'catch #(.catch ^js %1 %2)
                                    'let promised-let}}))

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
            _ (evaluate-code config sci-state editor-state)
            vars (->> (sci/eval-string "(->> *ns* ns-publics keys)" {:env sci-state})
                      (map #(vector % (sci/eval-string (str %) {:env sci-state}))))]
      (->> vars
           (filter (comp fn? second))
           (reduce (fn [acc [k fun]]
                     (assoc acc (-> k str keyword) {:name (name-for k)
                                                    :command #(catch-errors
                                                               fun editor-state)}))
                   {})))))

(declare prepare-commands)
(defn- watch-config [editor-state cmds-from-tooling config-file]
  (when config-file
    (let [dir (dirname config-file)
          watch-pid (watch dir
                           (fn [evt filename]
                             (when (= (join dir filename) config-file)
                               (prepare-commands editor-state cmds-from-tooling))))
          old-disconnect (-> @editor-state :editor/callbacks :on-disconnect)]
      (swap! editor-state assoc-in
             [:editor/callbacks :on-disconnect] (fn []
                                                   (.close ^js watch-pid)
                                                   (old-disconnect))))))

(defn prepare-commands [editor-state cmds-from-tooling]
  (p/let [config-file (-> @editor-state :editor/callbacks :config-file-path)
          cmds-from-config (fns-for editor-state config-file)
          commands (merge cmds-from-tooling cmds-from-config)]

    (watch-config editor-state cmds-from-tooling config-file)
    (swap! editor-state assoc :editor/commands commands)
    (cmds/run-callback! editor-state :register-commands commands)))
