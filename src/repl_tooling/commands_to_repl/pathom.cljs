(ns repl-tooling.commands-to-repl.pathom
  (:require [promesa.core :as p]
            [repl-tooling.editor-helpers :as helpers]
            [clojure.string :as str]
            [repl-tooling.editor-integration.commands :as cmds]
            [clojure.core.async :as async]
            [com.wsscode.pathom.core :as pathom]
            [com.wsscode.pathom.connect :as connect]))

(connect/defresolver editor-data [_ _]
  {::connect/output [:editor/contents :editor/filename :editor/range
                     :editor/additional-data]}
  (p/let [data (cmds/run-callback! (-> @chlorine.state/state :tooling-state)
                                   :editor-data)]
    {:editor/contents (:content data)
     :editor/filename (:filename data)
     :editor/range (:range data)
     :editor/additional-data (dissoc data [:content :filename :range])}))

(connect/defresolver namespace-from-editor-data [_ {:editor/keys [contents range]}]
  {::connect/input #{:editor/contents :editor/range}
   ::connect/output [:editor/ns-range :editor/namespace]}
  (let [[range ns] (helpers/ns-range-for contents (first range))]
    {:editor/ns-range range
     :editor/namespace (str ns)}))

(connect/defresolver namespace-from-editor
  [{:keys [editor-state]} {:editor/keys [namespace]}]
  {::connect/input #{:editor/namespace}
   ::connect/output [:repl/namespace]}

  {:repl/namespace (symbol namespace)})

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

(connect/defresolver all-vars-in-ns
  [{:keys [editor-state ast]} {:keys [repl/namespace]}]
  {::connect/input #{:repl/namespace}
   ::connect/output [{:namespace/vars [:var/fqn]}]}

  (p/let [{:keys [result]} (cmds/run-feature! editor-state :eval
                                              {:text (str "(ns-interns '" namespace ")")
                                               :ignore true
                                               :aux true})]
    {:namespace/vars (map (fn [v] {:var/fqn (symbol namespace v)})
                          (keys result))}))

#_
(eql (-> @chlorine.state/state :tooling-state)
     '[{(:repl/namespaces {:filter "chlorine.utils"})
        [:repl/namespace
         {:namespace/vars
          [:var/fqn (:var/meta {:keys [:file :line :macro? :macro]})]}]}])
     ; '[{(:repl/namespaces {:filter "chlorine"})
     ;    [:repl/namespace {:namespace/vars [:var/fqn (:var/meta {:keys [:file :row :line]})]}]}])

(connect/defresolver fqn-var
  [{:keys [editor-state]} {:keys [repl/namespace editor/symbol-name]}]
  {::connect/input #{:repl/namespace :editor/symbol-name}
   ::connect/output [:var/fqn]}

  (p/let [{:keys [result]} (cmds/run-feature! editor-state :eval
                                              {:namespace (str namespace)
                                               :ignore true
                                               :text (str "`" symbol-name)})]
    {:var/fqn result}))

(connect/defresolver meta-for-var
  [{:keys [editor-state ast]} {:var/keys [fqn]}]
  {::connect/input #{:var/fqn}
   ::connect/output [:var/meta]}

  (p/let [keys (-> ast :params :keys)
          {:keys [result]} (cmds/run-feature! editor-state :eval
                                              {:aux true
                                               :ignore true
                                               :text (str "(meta #'" fqn ")")})]
    {:var/meta (cond-> result
                       (coll? keys) (select-keys keys))}))

#_
(eql (-> @chlorine.state/state :tooling-state)
     '[{([:editor/namespace "promesa.core"]
         {:pathom/context {:editor/symbol-name "let"}})
        [(:var/meta {:keys [:file :row]})]}])


(def my-resolvers [;Editor resolvers
                   editor-data namespace-from-editor-data namespace-from-editor
                   ; Namespaces resolvers
                   all-namespaces all-vars-in-ns
                   ; Vars resolvers
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

(defn eql [editor-state query]
  (let [p (p/deferred)]
    (async/go
      (try
        (p/resolve! p (async/<! (parser {:editor-state editor-state}
                                  query)))
       (catch :default e
         (p/reject! p e))))
    p))
