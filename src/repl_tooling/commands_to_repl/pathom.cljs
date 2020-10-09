(ns repl-tooling.commands-to-repl.pathom
  (:require [promesa.core :as p]
            [schema.core :as s]
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

#_
(eql (-> @chlorine.state/state :tooling-state)
     '[:var/meta])

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

(connect/defresolver repls-from-config
  [{:keys [editor-state ast]} {:keys [editor/config]}]
  {::connect/input #{:editor/config}
   ::connect/output [:repl/eval :repl/aux :repl/clj]}

  (let [clj-eval (:clj/repl @editor-state)
        clj-aux (:clj/aux @editor-state)]
    (case (:eval-mode config)
      :clj {:repl/eval clj-eval
            :repl/aux clj-aux
            :repl/clj clj-aux}
      :cljs {:repl/eval (:cljs/repl @editor-state)
             :repl/aux (:cljs/repl @editor-state)
             :repl/clj clj-aux}
      nil)))

(connect/defresolver repls-from-config+editor-data
  [{:keys [editor-state ast]} {:keys [editor/config editor/filename]}]
  {::connect/input #{:editor/config :editor/filename}
   ::connect/output [:repl/eval :repl/aux :repl/clj]}

  (let [cljs-file? (str/ends-with? filename ".cljs")
        cljc-file? (or (str/ends-with? filename ".cljc")
                       (str/ends-with? filename ".cljx"))
        clj-aux (:clj/aux @editor-state)
        clj {:repl/eval (:clj/repl @editor-state)
             :repl/aux clj-aux
             :repl/clj clj-aux}
        cljs {:repl/eval (:cljs/repl @editor-state)
                         :repl/aux (:cljs/repl @editor-state)
                         :repl/clj clj-aux}]
    (cond
      (-> config :eval-mode (= :prefer-clj))
      (cond
        cljc-file? clj
        cljs-file? cljs
        :else clj)

      (-> config :eval-mode (= :prefer-cljs))
      (if cljc-file? cljs clj))))
    ; (case (:eval-mode config)
    ;   :prefer-clj {:repl/eval clj-eval
    ;                :repl/aux clj-aux
    ;                :repl/clj clj-aux}
    ;   :prefer-cljs {:repl/eval (:cljs/repl @editor-state)
    ;                 :repl/aux (:cljs/repl @editor-state)
    ;                 :repl/clj clj-aux}
    ;   nil)))

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
  [{:keys [editor-state]} {:keys [repl/namespace editor/current-var editor/filename]}]
  {::connect/input #{:repl/namespace :editor/current-var :editor/filename}
   ::connect/output [:var/fqn]}

  (p/let [{:keys [result]} (cmds/run-feature! editor-state :eval
                                              {:namespace (str namespace)
                                               :filename filename
                                               :ignore true
                                               :aux true
                                               :text (str "`" current-var)})]
    {:var/fqn result}))

#_
(eql (-> @chlorine.state/state :tooling-state)
     '[{[:editor/current-var "fqn-var"]
        [:var/meta]}])

(connect/defresolver cljs-env [{:keys [editor-state]} _]
  {::connect/output [:cljs/env]}

  (when-let [cmd (-> @editor-state :repl/info :cljs/repl-env)]
    (p/let [{:keys [result]} (cmds/run-feature! editor-state :eval
                                                {:ignore true
                                                 :aux :always
                                                 :text (str cmd)})]
      {:cljs/env result})))

(connect/defresolver get-config [{:keys [editor-state]} _]
  {::connect/output [:editor/config]}

  (p/let [cfg (cmds/run-callback! editor-state :get-config)]
    {:editor/config cfg}))

(connect/defresolver need-cljs [{:keys [editor-state]} {:editor/keys [config filename]}]
  {::connect/input #{:editor/config :editor/filename}
   ::connect/output [:cljs/required?]}

  {:cljs/required? (e-eval/need-cljs? config filename)})

#_
(eql (-> @chlorine.state/state :tooling-state)
     [:cljs/required?])

; (connect/defresolver meta-for-cljs-env [_ {:keys [var/meta]}]
;   {::connect/output [:var/meta]
;    ::connect/resolver-weights 200
;    ::connect/sort-plan (fn [ & args] (prn :SORTING args) 200)}
;
;   (prn :META meta)
;   {:var/meta {:lol "IMAMETA"}}
;   nil)
(connect/defresolver meta-for-var
  [{:keys [editor-state ast]} {:keys [var/fqn editor/filename cljs/required?]}]
  {::connect/input #{:var/fqn :editor/filename :cljs/required?}
   ::connect/output [:var/meta]}

  (p/let [keys (-> ast :params :keys)
          res (-> (cmds/run-feature! editor-state :eval
                                     {:aux true
                                      :ignore true
                                      :filename filename
                                      :text (str "(meta #'" fqn ")")})
                  (p/catch (constantly nil)))
          res (if (and required? (-> res :result nil?))
                (cmds/run-feature! editor-state :eval
                                 {:aux :always
                                  :ignore true
                                  :filename filename
                                  :text (str "(meta #'" fqn ")")})
                res)]
    {:var/meta (cond-> (:result res)
                       (coll? keys) (select-keys keys))}))

(def my-resolvers [;Editor resolvers
                   editor-data separate-data
                   namespace-from-editor-data namespace-from-editor var-from-editor
                   need-cljs get-config

                   ; Namespaces resolvers
                   all-namespaces all-vars-in-ns

                   ; REPLs resolvers
                   repls-from-config repls-from-config+editor-data

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
