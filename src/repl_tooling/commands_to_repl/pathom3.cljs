(ns repl-tooling.commands-to-repl.pathom3
  (:require [promesa.core :as p]
            [schema.core :as s]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.schemas :as schemas]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [clojure.string :as str]
            [repl-tooling.editor-integration.commands :as cmds]
            [clojure.core.async :as async]
            [com.wsscode.pathom3.interface.async.eql :as p.a.eql]
            [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.pathom3.connect.indexes :as pci]
    ; FIXME: Remove this
            [clojure.edn]
            ["child_process" :refer [spawn]]))

(pco/defresolver editor-data [{:keys [callbacks]} _]
  {::pco/output [:editor/data]}

  (p/let [data ((:editor-data callbacks))]
    {:editor/data data}))

(pco/defresolver separate-data [{:editor/keys [data]}]
  {::pco/output [:editor/contents :editor/filename :editor/range]}

  {:editor/contents (:contents data)
   :editor/filename (:filename data)
   :editor/range    (:range data)})

(pco/defresolver namespace-from-editor-data [{:editor/keys [contents range]}]
  {::pco/output [:editor/ns-range :editor/namespace]}

  (if-let [[range ns] (helpers/ns-range-for contents (first range))]
    {:editor/ns-range  range
     :editor/namespace (str ns)}
    {:editor/namespace nil}))

(pco/defresolver namespace-from-editor
  [{:keys [editor/namespace cljs/required?]}]
  {::pco/output [:repl/namespace]}

  (cond
    namespace {:repl/namespace (symbol namespace)}
    required? {:repl/namespace 'cljs.user}
    :else {:repl/namespace 'user}))

(pco/defresolver var-from-editor
  [{:editor/keys [contents range]}]
  {::pco/output [:editor/current-var :editor/current-var-range]}

  (when-let [[range curr-var] (helpers/current-var contents (first range))]
    {:editor/current-var       curr-var
     :editor/current-var-range range}))

(pco/defresolver all-namespaces
  [{:keys [ast]} {:keys [repl/clj]}]
  {::pco/output [{:repl/namespaces [:repl/namespace]}]}

  (p/let [f (-> ast :params :filter)
          {:keys [result]} (eval/eval clj "(clojure.core/mapv clojure.core/ns-name (clojure.core/all-ns))")]
    {:repl/namespaces (cond->> (map (fn [n] {:repl/namespace n}) result)
                        f (filter (fn [n]
                                    (-> n :repl/namespace str
                                        (str/starts-with? f)))))}))

(pco/defresolver need-cljs-from-config [{:editor/keys [config]}]
  {::pco/output [:cljs/required?]}

  (case (:eval-mode config)
    :clj {:cljs/required? false}
    :cljs {:cljs/required? true}
    nil))

(pco/defresolver need-cljs [{:editor/keys [config filename]}]
  {::pco/output [:cljs/required?]}

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
      {:cljs/required? (or cljs-file? cljc-file?)})))

(pco/defresolver repls-for-evaluation
  [{:keys [editor-state]} {:keys [cljs/required?]}]
  {::pco/output [:repl/eval :repl/aux :repl/clj]}

  (when-let [clj-aux (some-> editor-state deref :clj/aux)]
    (if required?
      (when-let [cljs (:cljs/repl @editor-state)]
        {:repl/eval cljs
         :repl/aux  cljs
         :repl/clj  clj-aux})
      {:repl/eval (:clj/repl @editor-state)
       :repl/aux  clj-aux
       :repl/clj  clj-aux})))

(pco/defresolver all-vars-in-ns
  [_ {:repl/keys [namespace aux]}]
  {::pco/output [{:namespace/vars [:var/fqn]}]}

  (p/let [{:keys [result]} (eval/eval aux (str "(clojure.core/ns-interns '" namespace ")"))]
    {:namespace/vars (map (fn [v] {:var/fqn (symbol namespace v)})
                       (keys result))}))

(pco/defresolver fqn-var
  [{:keys [repl/namespace editor/current-var editor/filename repl/aux]}]
  {::pco/output [:var/fqn]}

  (p/let [{:keys [result]} (eval/eval aux (str "`" current-var)
                             {:namespace (str namespace)
                              :ignore    true})]
    {:var/fqn result}))

(pco/defresolver cljs-env [{:keys [editor-state]} {:keys [repl/clj]}]
  {::pco/output [:cljs/env]}

  (when-let [cmd (-> @editor-state :repl/info :cljs/repl-env)]
    (p/let [{:keys [result]} (eval/eval clj (str cmd))]
      {:cljs/env result})))

(pco/defresolver get-config [{:keys [callbacks]} _]
  {::pco/output [:editor/config]}

  (p/let [cfg ((:get-config callbacks))]
    {:editor/config cfg}))

(pco/defresolver meta-for-var
  [{:keys [ast]} {:keys [var/fqn cljs/required? repl/aux repl/clj]}]
  {::pco/output [:var/meta]}

  (p/let [keys (-> ast :params :keys)
          res  (-> aux
                   (eval/eval (str "(clojure.core/meta #'" fqn ")"))
                   (p/catch (constantly nil)))
          res  (if (and required? (-> res :result nil?))
                 (eval/eval clj (str "(clojure.core/meta #'" fqn ")"))
                 res)]
    {:var/meta (cond-> (:result res)
                 (coll? keys) (select-keys keys))}))

(pco/defresolver spec-for-var
  [{:keys [var/fqn repl/aux]}]
  {::pco/output [:var/spec]}

  (p/let [{:keys [result]}
          (eval/eval
            aux
            (str "(clojure.core/let [s (clojure.spec.alpha/get-spec '" fqn ")"
              "                   fun #(clojure.core/some->> (% s) clojure.spec.alpha/describe)]"
              " (clojure.core/when s"
              "   (clojure.core/->> [:args :ret :fn]"
              "      (clojure.core/map (clojure.core/juxt clojure.core/identity fun))"
              "      (clojure.core/filter clojure.core/second)"
              "      (clojure.core/into {}))))"))]
    (when result {:var/spec result})))

(def ^:private kondo-cache (atom {:cache nil :when 0}))

(defn- run-kondo [dirs]
  (let [p      (p/deferred)
        buffer (atom "")
        cp     (spawn "clj-kondo"
                 (clj->js (concat ["--lint"]
                            dirs
                            ["--config"
                             "{:output {:analysis true :format :json}}"])))]
    (.. cp -stdout (on "data" #(swap! buffer str %)))
    (. cp on "error" #(p/resolve! p nil))
    (. cp on "close" #(p/resolve! p @buffer))
    p))

(defn- run-kondo-maybe [dirs]
  (let [curr-time (long (new js/Date))
        {:keys [when cache]} @kondo-cache]
    (if (< (- curr-time 6000) when)
      cache
      (p/finally (run-kondo dirs)
        (fn [res]
          (reset! kondo-cache {:when (int (new js/Date)) :cache res}))))))

(pco/defresolver analysis-from-kondo
  [{:keys [editor-state]} {:keys [editor/config]}]
  {::pco/output [:kondo/analysis]}

  (when-not editor-state
    (p/let [kondo (run-kondo-maybe (:project-paths config))]
      {:kondo/analysis (some-> (.parse js/JSON kondo) .-analysis)})))

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

(pco/defresolver fqn-from-kondo
  [{:keys [kondo/analysis editor/current-var repl/namespace]}]
  {::pco/output [:var/fqn]}

  (let [as-sym     (symbol current-var)
        ns-part    (clojure.core/namespace as-sym)
        without-ns (name as-sym)
        finding    (if ns-part
                     (get-from-ns-usages analysis namespace ns-part)
                     (or (get-from-var-usages analysis namespace current-var)
                         (some-> (get-from-definitions analysis namespace current-var)
                                 .-ns)))]
    (when finding
      {:var/fqn (symbol finding without-ns)})))

(pco/defresolver meta-from-kondo
  [{:keys [kondo/analysis var/fqn]}]
  {::pco/output [:var/meta]}

  (let [ns-part    (namespace fqn)
        without-ns (name fqn)]
    (when-let [^js res (get-from-definitions analysis ns-part without-ns)]
      {:var/meta (cond-> {:file   (.-filename res)
                          :line   (.-row res)
                          :column (.-col res)
                          :ns     (.-ns res) :name (.-name res)}
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
                   cljs-env fqn-var meta-for-var spec-for-var

                   ;; KONDO
                   analysis-from-kondo fqn-from-kondo meta-from-kondo])

(def env
  (pci/register my-resolvers))

(s/defn eql :- js/Promise
  "Queries the Pathom graph for the REPLs"
  [params :- {(s/optional-key :editor-state) schemas/EditorState
              (s/optional-key :callbacks)    s/Any}
   query]

  (let [params (cond-> params
                 (-> params :callbacks nil?)
                 (assoc :callbacks (-> params :editor-state deref :editor/callbacks)))]
    (p.a.eql/process (merge env params) query)))
