(ns repl-tooling.commands-to-repl.orchard
  (:require [repl-tooling.eval :as eval]
            [promesa.core :as p]))

(defn- have-ns? [repl namespace]
  (-> (eval/eval repl (str "(require '[" namespace "])"))
      (p/then (constantly true))
      (p/catch (constantly false))))

(defn- info! [repl editor-state]
  (prn :LOL!))

(defn cmds [editor-state]
  (p/let [aux-repl (:clj/aux @editor-state)
          _ (prn :AUX aux-repl)
          have-info? (have-ns? aux-repl "orchard.info")]
    (cond-> {}
            have-info? (assoc :info-for-var {:name "Info for var"
                                             :description "Gets information for the current var, under cursor"
                                             :command #(info! aux-repl editor-state)}))))
