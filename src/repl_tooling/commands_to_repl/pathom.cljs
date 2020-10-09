(ns repl-tooling.commands-to-repl.pathom
  (:require [promesa.core :as p]
            [schema.core :as s]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.schemas :as schemas]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [clojure.string :as str]
            [repl-tooling.editor-integration.commands :as cmds]
            [clojure.core.async :as async]
            [com.wsscode.pathom.core :as pathom]
            [com.wsscode.pathom.connect :as connect]))

(connect/defresolver editor-data [{:keys [editor-state]} _]
  {::connect/output [:editor/data]}

  (p/let [data (cmds/run-callback! editor-state :editor-data)]
    {:editor/data data}))

(connect/defresolver separate-data [_ {:editor/keys [data]}]
  {::connect/input #{:editor/data}
   ::connect/output [:editor/contents :editor/filename :editor/range]}

  {:editor/contents (:contents data)
   :editor/filename (:filename data)
   :editor/range (:range data)})

(connect/defresolver namespace-from-editor-data [_ {:editor/keys [contents range]}]
  {::connect/input #{:editor/contents :editor/range}
   ::connect/output [:editor/ns-range :editor/namespace]}

  (when-let [[range ns] (helpers/ns-range-for contents (first range))]
    {:editor/ns-range range
     :editor/namespace (str ns)}))

(connect/defresolver namespace-from-editor
  [{:keys [editor-state]} {:editor/keys [namespace]}]
  {::connect/input #{:editor/namespace}
   ::connect/output [:repl/namespace]}

  {:repl/namespace (symbol namespace)})

(connect/defresolver var-from-editor
  [{:keys [editor-state]} {:editor/keys [contents range]}]
  {::connect/input #{:editor/contents :editor/range}
   ::connect/output [:editor/current-var :editor/current-var-range]}

  (when-let [[range curr-var] (helpers/current-var contents (first range))]
    {:editor/current-var curr-var
     :editor/current-var-range range}))

(connect/defresolver all-namespaces
  [{:keys [editor-state ast]} _]
  {::connect/output [{:repl/namespaces [:repl/namespace]}]}

  (p/let [f (-> ast :params :filter)
          {:keys [result]} (cmds/run-feature! editor-state :eval
                                              {:aux true
                                               :ignore true
                                               :text "(map ns-name (all-ns))"})]
    {:repl/namespaces (cond->> (map (fn [n] {:repl/namespace n}) result)
                               f (filter (fn [n]
                                           (-> n :repl/namespace str
                                               (str/starts-with? f)))))}))

(connect/defresolver need-cljs-from-config [_ {:editor/keys [config]}]
  {::connect/input #{:editor/config}
   ::connect/output [:cljs/required?]}

  (case (:eval-mode config)
    :clj {:cljs/required? false}
    :cljs {:cljs/required? true}
    nil))

(connect/defresolver need-cljs [{:keys [editor-state]} {:editor/keys [config filename]}]
  {::connect/input #{:editor/config :editor/filename}
   ::connect/output [:cljs/required?]}

  (let [cljs-file? (str/ends-with? filename ".cljs")
        cljc-file? (or (str/ends-with? filename ".cljc")
                       (str/ends-with? filename ".cljx"))]
    (cond
      (-> config :eval-mode (= :prefer-clj))
      (cond
        cljc-file? {:cljs/required? false}
        cljs-file? {:cljs/required? true}
        :else {:cljs/required? false})

      (-> config :eval-mode (= :prefer-cljs))
      {:cljs/required? cljc-file?})))

(connect/defresolver repls-for-evaluation
  [{:keys [editor-state ast]} {:keys [:cljs/required?]}]
  {::connect/input #{:cljs/required?}
   ::connect/output [:repl/eval :repl/aux :repl/clj]}

  (let [clj-aux (:clj/aux @editor-state)]
    (if required?
      {:repl/eval (:cljs/repl @editor-state)
       :repl/aux (:cljs/repl @editor-state)
       :repl/clj clj-aux}
      {:repl/eval (:clj/repl @editor-state)
       :repl/aux clj-aux
       :repl/clj clj-aux})))

(connect/defresolver all-vars-in-ns
  [{:keys [editor-state ast]} {:keys [repl/namespace repl/aux]}]
  {::connect/input #{:repl/namespace :repl/aux}
   ::connect/output [{:namespace/vars [:var/fqn]}]}

  (p/let [{:keys [result]} (eval/eval aux (str "(ns-interns '" namespace ")"))]
          ; (cmds/run-feature! editor-state :eval
          ;                    {:text (str "(ns-interns '" namespace ")")
          ;                     :ignore true
          ;                     :aux true})]
    {:namespace/vars (map (fn [v] {:var/fqn (symbol namespace v)})
                          (keys result))}))

#_
(eql (-> @chlorine.state/state :tooling-state)
     '[{(:repl/namespaces {:filter "chlorine.utils"})
        [:repl/namespace
         {:namespace/vars
          [:var/fqn (:var/meta {:keys [:file :line :macro? :macro]})]}]}])

(connect/defresolver fqn-var
  [{:keys [editor-state]} {:keys [repl/namespace editor/current-var editor/filename
                                  repl/aux]}]
  {::connect/input #{:repl/namespace :editor/current-var :editor/filename :repl/aux}
   ::connect/output [:var/fqn]}

  (p/let [{:keys [result]} (eval/eval aux (str "`" current-var)
                                      {:namespace (str namespace)
                                       :ignore true})]
    {:var/fqn result}))

#_
(eql (-> @chlorine.state/state :tooling-state)
     '[{[:editor/current-var "fqn-var"]
        [:var/meta]}])

(connect/defresolver cljs-env [{:keys [editor-state]} {:keys [repl/clj]}]
  {::connect/input #{:repl/clj}
   ::connect/output [:cljs/env]}

  (when-let [cmd (-> @editor-state :repl/info :cljs/repl-env)]
    (p/let [{:keys [result]} (eval/eval clj (str cmd))]
      {:cljs/env result})))

(connect/defresolver get-config [{:keys [editor-state]} _]
  {::connect/output [:editor/config]}

  (p/let [cfg (cmds/run-callback! editor-state :get-config)]
    {:editor/config cfg}))

#_
(eql (-> @chlorine.state/state :tooling-state)
     [:cljs/required?])

(connect/defresolver meta-for-var
  [{:keys [editor-state ast]} {:keys [var/fqn editor/filename cljs/required?
                                      repl/aux repl/clj]}]
  {::connect/input #{:var/fqn :editor/filename :cljs/required? :repl/aux :repl/clj}
   ::connect/output [:var/meta]}

  (p/let [keys (-> ast :params :keys)
          res (-> aux
                  (eval/eval (str "(meta #'" fqn ")"))
                  (p/catch (constantly nil)))
          res (if (and required? (-> res :result nil?))
                (eval/eval clj (str "(meta #'" fqn ")"))
                res)]
    {:var/meta (cond-> (:result res)
                       (coll? keys) (select-keys keys))}))

(def my-resolvers [;Editor resolvers
                   editor-data separate-data
                   namespace-from-editor-data namespace-from-editor var-from-editor
                   get-config

                   ; Namespaces resolvers
                   all-namespaces all-vars-in-ns

                   ; REPLs resolvers
                   need-cljs need-cljs-from-config
                   ; repls-from-config repls-from-config+editor-data
                   repls-for-evaluation

                   ; Vars resolvers
                   cljs-env
                   fqn-var meta-for-var])

(def parser
  (pathom/async-parser
    {::pathom/env {::pathom/reader [pathom/map-reader
                                    connect/async-reader2
                                    connect/open-ident-reader
                                    connect/index-reader
                                    pathom/env-placeholder-reader]
                   ::pathom/placeholder-prefixes #{">"}}
     ::pathom/plugins [(connect/connect-plugin {::connect/register my-resolvers})
                       pathom/error-handler-plugin
                       pathom/trace-plugin]}))

(s/defn eql :- js/Promise [editor-state :- schemas/EditorState, query]
  (let [p (p/deferred)]
    (async/go
      (try
        (p/resolve! p (async/<! (parser {:editor-state editor-state}
                                  query)))
       (catch :default e
         (p/reject! p e))))
    p))
