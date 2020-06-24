(ns repl-tooling.commands-to-repl.orchard
  (:require [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clj-helper :as h]
            [repl-tooling.editor-helpers :as helpers]
            [clojure.string :as str]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.editor-integration.commands :as cmds]
            [repl-tooling.repl-client.source :as source]
            [promesa.core :as p]))

(defn- have-ns? [repl namespace]
  (-> (eval/eval repl (source/have-ns-command namespace))
      (p/then :result)
      (p/catch (constantly false))))

(def ^:private info-msg (h/contents-for-fn "orchard-cmds.clj" "info"))

(defn- info! [repl editor-state]
  (p/let [{:keys [on-start-eval on-eval editor-data get-config]}
          (:editor/callbacks @editor-state)
          {:keys [contents range filename] :as ed} (editor-data)
          start (first range)
          id (gensym "info")
          [range var] (helpers/current-var contents start)
          full-var-name (cmds/run-feature! editor-state :eval
                                           {:text (str "`" var)
                                            :ignore true
                                            :auto-detect true :aux true})
          splitted (-> full-var-name :result str (str/split #"/" 2))
          [ns-name name] (cond->> splitted
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

(def ^:private xref-msg (h/contents-for-fn "orchard-cmds.clj" "find-usages"))
(defn- xref! [editor-state]
  (p/let [fqn (cmds/run-feature! editor-state :get-full-var-name)
          cmd (str "(" xref-msg " " (-> fqn :result str pr-str) ")")]
    (cmds/run-feature! editor-state :eval-and-render
                       cmd (:range fqn) {:aux true :interactive true})))

(def ^:private doc-msg (h/contents-for-fn "orchard-cmds.clj" "clojure-docs"))
(defn- cljdoc! [editor-state]
  (p/let [fqn (cmds/run-feature! editor-state :get-full-var-name)
          s (-> fqn :result str (str/split #"/" 2))
          [ns-name var-name] (cond->> s (-> s count (= 1)) (cons ""))
          cmd (str "(" doc-msg " " (pr-str ns-name) " " (pr-str var-name) ")")]
    (cmds/run-feature! editor-state :eval-and-render
                       cmd (:range fqn) {:aux :always :interactive true})))

(defn cmds [editor-state]
  (p/let [aux-repl (:clj/aux @editor-state)
          have-info? (have-ns? aux-repl "orchard.info")
          have-xref? (have-ns? aux-repl "orchard.xref")
          have-docs? (have-ns? aux-repl "orchard.clojuredocs")]
    (cond-> {}
            have-info? (assoc :info-for-var {:name "Info for var"
                                             :description "Gets information for the current var, under cursor"
                                             :command #(info! aux-repl editor-state)})
            have-xref? (assoc :find-usages {:name "Find usages"
                                            :description "Find usages of the current var"
                                            :command #(xref! editor-state)})
            have-docs? (assoc :clojure-doc-for-var {:name "Clojure doc for var"
                                                    :description "Find the Clojure doc of the current var"
                                                    :command #(cljdoc! editor-state)}))))
