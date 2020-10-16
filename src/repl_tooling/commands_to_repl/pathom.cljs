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
            [com.wsscode.pathom.connect :as connect]
            ; FIXME: Remove this
            [clojure.edn]
            ["child_process" :refer [spawn]]))

(connect/defresolver editor-data [{:keys [callbacks]} _]
  {::connect/output [:editor/data]}

  (p/let [data ((:editor-data callbacks))]
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
  [_ {:editor/keys [namespace]}]
  {::connect/input #{:editor/namespace}
   ::connect/output [:repl/namespace]}

  {:repl/namespace (symbol namespace)})

(connect/defresolver var-from-editor
  [_ {:editor/keys [contents range]}]
  {::connect/input #{:editor/contents :editor/range}
   ::connect/output [:editor/current-var :editor/current-var-range]}

  (when-let [[range curr-var] (helpers/current-var contents (first range))]
    {:editor/current-var curr-var
     :editor/current-var-range range}))

(connect/defresolver all-namespaces
  [{:keys [editor-state ast]} {:keys [:repl/clj]}]
  {::connect/input #{:repl/clj}
   ::connect/output [{:repl/namespaces [:repl/namespace]}]}

  (p/let [f (-> ast :params :filter)
          {:keys [result]} (eval/eval clj "(mapv ns-name (all-ns))")]
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

(connect/defresolver need-cljs [_ {:editor/keys [config filename]}]
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

  (when-let [clj-aux (some-> editor-state deref :clj/aux)]
    (if required?
      {:repl/eval (:cljs/repl @editor-state)
       :repl/aux (:cljs/repl @editor-state)
       :repl/clj clj-aux}
      {:repl/eval (:clj/repl @editor-state)
       :repl/aux clj-aux
       :repl/clj clj-aux})))

(connect/defresolver all-vars-in-ns
  [_ {:keys [repl/namespace repl/aux]}]
  {::connect/input #{:repl/namespace :repl/aux}
   ::connect/output [{:namespace/vars [:var/fqn]}]}

  (p/let [{:keys [result]} (eval/eval aux (str "(ns-interns '" namespace ")"))]
    {:namespace/vars (map (fn [v] {:var/fqn (symbol namespace v)})
                          (keys result))}))

(connect/defresolver fqn-var
  [_ {:keys [repl/namespace editor/current-var editor/filename
             repl/aux]}]
  {::connect/input #{:repl/namespace :editor/current-var :editor/filename :repl/aux}
   ::connect/output [:var/fqn]}

  (p/let [{:keys [result]} (eval/eval aux (str "`" current-var)
                                      {:namespace (str namespace)
                                       :ignore true})]
    {:var/fqn result}))

(connect/defresolver cljs-env [{:keys [editor-state]} {:keys [repl/clj]}]
  {::connect/input #{:repl/clj}
   ::connect/output [:cljs/env]}

  (when-let [cmd (-> @editor-state :repl/info :cljs/repl-env)]
    (p/let [{:keys [result]} (eval/eval clj (str cmd))]
      {:cljs/env result})))

(connect/defresolver get-config [{:keys [callbacks]} _]
  {::connect/output [:editor/config]}

  (p/let [cfg ((:get-config callbacks))]
    {:editor/config cfg}))

(connect/defresolver meta-for-var
  [{:keys [ast]} {:keys [var/fqn cljs/required? repl/aux repl/clj]}]
  {::connect/input #{:var/fqn :cljs/required? :repl/aux :repl/clj}
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

(defn- run-kondo [dirs]
  (let [p (p/deferred)
        buffer (atom "")
        cp
        (spawn "clj-kondo" (clj->js (concat ["--lint"]
                                            dirs
                                            ["--config"
                                             "{:output {:analysis true :format :json}}"])))]
    (.. cp -stdout (on "data" #(swap! buffer str %)))
    (. cp on "close" #(p/resolve! p @buffer))
    p))

(connect/defresolver analysis-from-kondo
  [{:keys [callbacks editor-state ast]} {:keys [editor/config]}]
  {::connect/input #{:editor/config}
   ::connect/output [:kondo/analysis]}

  (when-not editor-state
    (p/let [kondo (run-kondo (:project-paths config))]
      {:kondo/analysis (.-analysis (.parse js/JSON kondo))})))

(defn- get-from-ns-usages [analysis namespace ns-part]
  (-> analysis
      (aget "namespace-usages")
      (->> (filter (fn [^js %] (and (-> % .-from (= (str namespace)))
                                    (-> % .-alias (= ns-part))))))
      first
      (some-> .-to)))

(defn- get-from-var-usages [analysis namespace current-var]
  (-> analysis
      (aget "var-usages")
      (->> (filter (fn [^js %] (and (-> % .-from (= (str namespace)))
                                    (-> % .-name (= current-var))))))
      first
      (some-> .-to)))

(defn- get-from-definitions [analysis namespace current-var]
  (-> analysis
      (aget "var-definitions")
      (->> (filter (fn [^js %] (and (-> % .-ns (= (str namespace)))
                                    (-> % .-name (= current-var))))))
      first))

(connect/defresolver fqn-from-kondo
  [_ {:keys [kondo/analysis editor/current-var repl/namespace]}]
  {::connect/input #{:kondo/analysis :editor/current-var :repl/namespace}
   ::connect/output [:var/fqn]}

  (let [as-sym (symbol current-var)
        ns-part (clojure.core/namespace as-sym)
        without-ns (name as-sym)
        finding (if ns-part
                  (get-from-ns-usages analysis namespace ns-part)
                  (or (get-from-var-usages analysis namespace current-var)
                      (some-> (get-from-definitions analysis namespace current-var)
                              .-ns)))]
    (when finding
      {:var/fqn (symbol finding without-ns)})))

(connect/defresolver meta-from-kondo
  [_ {:keys [kondo/analysis var/fqn]}]
  {::connect/input #{:kondo/analysis :var/fqn}
   ::connect/output [:var/meta]}

  (let [ns-part (namespace fqn)
        without-ns (name fqn)]
    (when-let [^js res (get-from-definitions analysis ns-part without-ns)]
      {:var/meta (cond-> {:file (.-filename res)
                          :line (.-row res)
                          :column (.-col res)
                          :ns (.-ns res) :name (.-name res)}
                         (.-doc res) (assoc :doc (.-doc res))
                         (.-test res) (assoc :test (.-test res)))})))

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
                   cljs-env fqn-var meta-for-var

                   ;; KONDO
                   analysis-from-kondo fqn-from-kondo meta-from-kondo])

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

(s/defn eql :- js/Promise
  "Queries the Pathom graph for the REPLs"
  [params :- {(s/optional-key :editor-state) schemas/EditorState
              (s/optional-key :callbacks) s/Any}
   query]
  (let [p (p/deferred)
        params (cond-> params

                       (-> params :callbacks nil?)
                       (assoc :callbacks (-> params :editor-state deref :editor/callbacks)))]
    (async/go
      (try
        (p/resolve! p (async/<! (parser params query)))
       (catch :default e
         (p/reject! p e))))
    p))
