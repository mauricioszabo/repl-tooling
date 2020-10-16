(ns repl-tooling.editor-integration.definition
  (:require [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.features.definition :as def]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.editor-integration.commands :as cmds]
            [promesa.core :as p]
            [clojure.string :as str]
            ["os" :refer [platform]]))

(defn goto-var [{:keys [var-name namespace repl state]}]
  (when-not
    (some-> repl (def/find-var-definition (:clj/aux @state) namespace var-name)
            (.then #(cmds/run-callback! state :open-editor %)))
    (cmds/run-callback! state
                        :notify
                        {:type :error :title "Could not find definition for var"})))

(defn goto-current-var [editor-data state]
  (let [{:keys [contents range editor filename]} editor-data
        [_ var] (helpers/current-var contents (first range))
        [_ namespace] (helpers/ns-range-for contents (first range))
        repl (e-eval/repl-for state filename true)]
    (goto-var {:var-name var :namespace namespace :repl repl :state state})))

(defn- norm-result [file-name]
  (cond-> file-name
          (and (re-find #"^win\d+" (platform)))
          (str/replace-first #"^/" "")))

(defn- resolve-var-path [state]
  (p/let [{:keys [run-feature run-callback]} @state
          {:keys [var/meta editor/data]} (run-feature :eql [:editor/data :var/meta])
          meta (select-keys meta [:file :line :column])
          k [:editor/data data]
          repl-res (run-feature :eql [{k [:repl/clj]}])
          repl (get-in repl-res [k :repl/clj])
          result (def/resolve-possible-path repl meta)]
    (cond-> (dissoc result :file)
            (:file-name result) (update :file-name norm-result)
            (:column result) (update :column dec))))

(defn goto-current-var' [state]
  (let [{:keys [run-callback]} @state]
    (-> (resolve-var-path state)
        (p/then #(run-callback :open-editor %))
        (p/catch #(run-callback :notify
                                {:type :error :title "Could not find definition for var"})))))
