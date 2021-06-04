(ns repl-tooling.editor-integration.configs
  (:require [sci.core :as sci]
            [promesa.core :as p]
            [clojure.string :as str]
            [repl-tooling.editor-integration.commands :as cmds]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.repl-client.clj-helper :refer [contents-for-fn]]
            [repl-tooling.ui.pinkie :as pinkie]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [repl-tooling.editor-integration.interpreter :as int]
            [repl-tooling.editor-integration.renderer :as render]
            [repl-tooling.commands-to-repl.pathom :as pathom]
            [repl-tooling.editor-integration.renderer.pinkie :as r-pinkie]
            ["highlight.js" :as highlight]
            ["commonmark" :refer [Parser HtmlRenderer]]
            ["path" :refer [dirname join]]
            ["fs" :refer [watch readFile existsSync]]
            ["ansi_up" :default Ansi]))

(defn- read-config-file [config-file]
  (let [p (p/deferred)]
    (readFile config-file #(p/resolve! p (str %2)))
    p))

(defn- name-for [k]
  (-> k name str/capitalize (str/replace #"-" " ")))

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
            _ (do (int/evaluate-code {:code config
                                      :sci-state sci-state
                                      :editor-state editor-state})
                nil)
            vars (->> (sci/eval-string "(->> *ns* ns-publics keys)" {:env sci-state})
                      (map #(vector % (sci/eval-string (str %) {:env sci-state})))
                      (remove (comp '#{println print prn log pr} first)))]
      (->> vars
           (filter (fn [[k v]] (fn? v)))
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

(defn- fns-or-check-errors [editor-state config-file]
  (p/catch (fns-for editor-state config-file)
           (fn [error]
             (let [serialized (pr-str (tagged-literal 'error
                                                      {:type (.-name error)
                                                       :data (.-data error)
                                                       :message (.-message error)
                                                       :trace (-> error .-stack str/split-lines)}))
                   data {:id (gensym "sci-error-")
                         :result (helpers/parse-result {:error serialized
                                                        :as-text serialized})}
                   error-data (:data error)
                   data (if (-> error-data :type (= :sci/error))
                          (let [rowcol [(-> error-data :line dec)
                                        (-> error-data :column dec)]]
                            (assoc data
                                   :range [rowcol rowcol]
                                   :editor-data {:filename config-file
                                                 :contents ""
                                                 :range [rowcol rowcol]}))
                          (assoc data
                                 :range [[0 0] [0 0]]
                                 :editor-data {:filename nil
                                               :contents ""
                                               :range [[0 0] [0 0]]}))]
               (cmds/run-callback! editor-state :on-start-eval (dissoc data :result))
               (cmds/run-callback! editor-state :on-eval (assoc data :repl nil)))
             nil)))

(defn- reg-commands [editor-state cmds-from-tooling config-file]
  (pathom/reset-resolvers)
  (p/let [cmds-from-config (fns-or-check-errors editor-state config-file)
          commands (-> cmds-from-tooling
                       (merge cmds-from-config)
                       (dissoc :let :then :catch))]
    (swap! editor-state assoc :editor/commands commands)
    (cmds/run-callback! editor-state :register-commands commands)))

(defn- clojure-renderer [editor-state edn]
  (let [fake-res {:result edn :parsed? true :as-text (pr-str edn)}
        parsed-ratom (render/parse-result fake-res nil editor-state)]
    [render/view-for-result parsed-ratom]))

(defn register-custom-tags! [editor-state]
  (pinkie/register-tag :div/ansi r-pinkie/ansi-tag)
  (pinkie/register-tag :div/clj #(clojure-renderer editor-state %))
  (r-pinkie/register-tag :div/md r-pinkie/markdown-tag)
  (pinkie/register-tag :div/clj-code r-pinkie/code-tag))

(defn prepare-commands [editor-state cmds-from-tooling]
  (register-custom-tags! editor-state)
  (swap! editor-state assoc :editor/commands cmds-from-tooling)
  (p/let [config-file (-> @editor-state :editor/callbacks :config-file-path)]
    (watch-config editor-state cmds-from-tooling config-file)
    (reg-commands editor-state cmds-from-tooling config-file)))
