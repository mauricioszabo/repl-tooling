(ns repl-tooling.commands-to-repl.pathom
  (:require [promesa.core :as p]
            [duck-repled.core :as duck]
            [duck-repled.repl-protocol :as duck-repl]
            [repl-tooling.eval :as eval]))

(def ^:private global-eql (atom nil))
(def ^:private global-resolvers (atom nil))
(def ^:private orig-resolvers (atom nil))

(defn reset-resolvers []
  (reset! global-resolvers @orig-resolvers)
  (reset! global-eql (duck/gen-eql @orig-resolvers)))

(defn add-resolver [config fun]
  (let [old @global-resolvers
        new (duck/add-resolver old config fun)]
    (reset! global-resolvers new)
    (reset! global-eql (duck/gen-eql new))))

(defn compose-resolver [config fun]
  (let [old @global-resolvers
        new (duck/compose-resolver old config fun)]
    (reset! global-resolvers new)
    (reset! global-eql (duck/gen-eql new))))

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

(defn eql-from-state [editor-state]
  (let [resolver #(resolvers-from-state editor-state)
        resolvers (duck/add-resolver {:inputs []
                                      :outputs [:editor/data :config/eval-as
                                                :config/project-paths :config/repl-kind]}
                                     resolver)]
    (reset! orig-resolvers resolvers)
    (reset! global-resolvers resolvers)
    (reset! global-eql (duck/gen-eql resolvers))
    (fn eql
      ([query] (@global-eql {} query))
      ([seed query] (@global-eql seed query)))))
