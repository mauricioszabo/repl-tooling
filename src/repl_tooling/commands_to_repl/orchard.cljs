(ns repl-tooling.commands-to-repl.orchard
  (:require-macros [repl-tooling.repl-client.clj-helper :as h])
  (:require [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]
            [clojure.string :as str]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [promesa.core :as p]))

(defn- have-ns? [repl namespace]
  (-> (eval/eval repl (str "(require '[" namespace "])"))
      (p/then (constantly true))
      (p/catch (constantly false))))

(def ^:private info-msg (h/contents-for-fn "orchard-cmds.clj" "info"))

(defn- info! [repl editor-state]
  (p/let [{:keys [on-start-eval on-eval editor-data get-config]}
          (:editor/callbacks @editor-state)
          evaluate (-> @editor-state :editor/features :eval)

          {:keys [contents range filename] :as ed} (editor-data)
          start (first range)
          id (gensym "info")
          [range var] (helpers/current-var contents start)
          full-var-name (evaluate (str "`" var) {:ignore true :auto-detect true :aux true})
          splitted (-> full-var-name :result str (str/split #"/" 2))
          [ns-name name] (cond-> splitted
                                 (= 1 (count splitted)) (cons "user"))

          params {:id id :editor-data ed :range range}
          cljs? (e-eval/need-cljs? (get-config) filename)
          cmd (str "(" info-msg " "
                   (pr-str ns-name) " "
                   (pr-str name) " '"
                   (pr-str {:dialect (if cljs? :cljs :clj)
                            :env (-> @editor-state :repl/info :cljs/repl-env)})
                   ")")
          [row col] start]
    (on-start-eval params)
    (eval/evaluate repl
                   cmd
                   {:ignore true :row row :col col}
                   #(on-eval (assoc params :result % :repl repl)))))

(defn cmds [editor-state]
  (p/let [aux-repl (:clj/aux @editor-state)
          have-info? (have-ns? aux-repl "orchard.info")]
    (cond-> {}
            have-info? (assoc :info-for-var {:name "Info for var"
                                             :description "Gets information for the current var, under cursor"
                                             :command #(info! aux-repl editor-state)}))))
