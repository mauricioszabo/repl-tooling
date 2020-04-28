(ns repl-tooling.commands-to-repl.orchard
  (:require-macros [repl-tooling.repl-client.clj-helper :as h])
  (:require [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]
            [promesa.core :as p]))

(defn- have-ns? [repl namespace]
  (-> (eval/eval repl (str "(require '[" namespace "])"))
      (p/then (constantly true))
      (p/catch (constantly false))))

(def ^:private info-msg (h/contents-for-fn "orchard-cmds.clj" "info"))

(defn- info! [repl editor-state]
  (p/let [{:keys [on-start-eval on-eval editor-data]} (:editor/callbacks @editor-state)
          {:keys [contents range] :as ed} (editor-data)
          eval-code (-> @editor-state :editor/features :eval-and-render)
          start (first range)
          id (gensym "info")
          [_ ns-name] (helpers/ns-range-for contents start)
          ns-name (or ns-name (p/then (eval/eval repl "(str clojure.core/*ns*)") :result))
          [range var] (helpers/current-var contents start)
          params {:id id :editor-data ed :range range}
          cmd (str "(" info-msg " " (pr-str (str ns-name)) " " (pr-str var) ")")
          [row col] start]
    (on-start-eval params)
    (eval/evaluate repl
                   cmd
                   {:ignore true :namespace ns-name :row row :col col}
                   #(on-eval (assoc params :result %)))))

(defn cmds [editor-state]
  (p/let [aux-repl (:clj/aux @editor-state)
          have-info? (have-ns? aux-repl "orchard.info")]
    (cond-> {}
            have-info? (assoc :info-for-var {:name "Info for var"
                                             :description "Gets information for the current var, under cursor"
                                             :command #(info! aux-repl editor-state)}))))
