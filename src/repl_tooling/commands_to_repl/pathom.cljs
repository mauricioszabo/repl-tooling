(ns repl-tooling.commands-to-repl.pathom
  (:require [promesa.core :as p]
            [schema.core :as s]
            [repl-tooling.eval :as eval]
            [clojure.set :as set]
            [repl-tooling.editor-integration.schemas :as schemas]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [clojure.string :as str]
            [repl-tooling.editor-integration.commands :as cmds]
            [clojure.core.async :as async]
            [com.wsscode.pathom.core :as pathom]
            [com.wsscode.pathom.connect :as connect]
            [repl-tooling.features.definition :as def]
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
  {::connect/output [:editor/ns-range :editor/namespace]}

  (if-let [[range ns] (helpers/ns-range-for contents (first range))]
    {:editor/ns-range range
     :editor/namespace (str ns)}
    {:editor/namespace nil}))

(connect/defresolver namespace-from-editor
  [_ {:keys [editor/namespace cljs/required?]}]
  {::connect/output [:repl/namespace]}

  (cond
    namespace {:repl/namespace (symbol namespace)}
    required? {:repl/namespace 'cljs.user}
    :else {:repl/namespace 'user}))

(connect/defresolver var-from-editor
  [_ {:editor/keys [contents range]}]
  {::connect/output [:editor/current-var :editor/current-var-range]}

  (when-let [[range curr-var] (helpers/current-var contents (first range))]
    {:editor/current-var curr-var
     :editor/current-var-range range}))

(connect/defresolver all-namespaces
  [{:keys [editor-state ast]} {:repl/keys [clj]}]
  {::connect/output [{:repl/namespaces [:repl/namespace]}]}

  (p/let [f (-> ast :params :filter)
          {:keys [result]} (eval/eval clj "(clojure.core/mapv clojure.core/ns-name (clojure.core/all-ns))")]
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
      {:cljs/required? (or cljs-file? cljc-file?)})))

(connect/defresolver repls-for-evaluation
  [{:keys [editor-state ast]} {:keys [:cljs/required?]}]
  {::connect/input #{:cljs/required?}
   ::connect/output [:repl/eval :repl/aux :repl/clj]}

  (when-let [clj-aux (some-> editor-state deref :clj/aux)]
    (if required?
      (when-let [cljs (:cljs/repl @editor-state)]
        {:repl/eval cljs
         :repl/aux cljs
         :repl/clj clj-aux})
      {:repl/eval (:clj/repl @editor-state)
       :repl/aux clj-aux
       :repl/clj clj-aux})))

(connect/defresolver all-vars-in-ns
  [_ {:keys [repl/namespace repl/aux]}]
  {::connect/input #{:repl/namespace :repl/aux}
   ::connect/output [{:namespace/vars [:var/fqn]}]}

  (p/let [{:keys [result]} (eval/eval aux (str "(clojure.core/ns-interns '" namespace ")"))]
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
    (when (symbol? result)
      {:var/fqn result})))

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
                  (eval/eval (str "(clojure.core/meta #'" fqn ")"))
                  (p/catch (constantly nil)))
          res (if (and required? (-> res :result nil?))
                (eval/eval clj (str "(clojure.core/meta #'" fqn ")"))
                res)]
    {:var/meta (cond-> (:result res)
                       (coll? keys) (select-keys keys))}))

(connect/defresolver spec-for-var
  [{:keys [ast]} {:keys [var/fqn repl/aux]}]
  {::connect/input #{:var/fqn :repl/aux}
   ::connect/output [:var/spec]}

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
  (let [p (p/deferred)
        buffer (atom "")
        cp (spawn "clj-kondo"
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

(connect/defresolver analysis-from-kondo
  [{:keys [callbacks editor-state ast]} {:keys [editor/config]}]
  {::connect/input #{:editor/config}
   ::connect/output [:kondo/analysis]}

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

(def orig-resolvers [;Editor resolvers
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
                     cljs-env fqn-var meta-for-var spec-for-var def/resolver

                     ;; KONDO
                     analysis-from-kondo fqn-from-kondo meta-from-kondo])

(defn gen-parser [resolvers]
  (pathom/async-parser
    {::pathom/env {::pathom/reader [pathom/map-reader
                                    connect/async-reader2
                                    connect/open-ident-reader
                                    connect/index-reader
                                    pathom/env-placeholder-reader]
                   ::pathom/placeholder-prefixes #{">"}}
     ::pathom/plugins [(connect/connect-plugin {::connect/register resolvers})
                       pathom/error-handler-plugin
                       pathom/trace-plugin]}))

(def ^:private custom-resolvers (atom []))
(def ^:private parser (atom (gen-parser orig-resolvers)))

(defn reset-resolvers []
  (reset! custom-resolvers [])
  (reset! parser (gen-parser orig-resolvers)))

(defn- rename-resolve-out [resolve-out]
  (let [out-ns (namespace resolve-out)
        out-name (name resolve-out)]
    (keyword out-ns (str out-name "-rewrote"))))

(defn- rename-resolvers-that-output [outputs]
  (let [rewroted-map (zipmap outputs (map rename-resolve-out outputs))]
    (for [resolver orig-resolvers
          :let [resolver-out (::connect/output resolver)
                new-out (mapv #(cond-> % (rewroted-map %) rewroted-map)
                              resolver-out)
                fun (::connect/resolve resolver)]]
      (assoc resolver
             ::connect/output new-out
             ::connect/resolve (fn [ & args]
                                 (p/let [res (apply fun args)]
                                   (set/rename-keys res rewroted-map)))))))

(defn- gen-resolver [inputs outputs fun]
  {::connect/syn (gensym "custom-resolver-")
   ::connect/resolve fun
   ::connect/output (vec outputs)
   ::connect/input (set inputs)})

(defn add-resolver [{:keys [inputs outputs]} fun]
  (swap! custom-resolvers conj (gen-resolver inputs outputs (fn [_ input] (fun input))))
  (reset! parser (gen-parser (concat orig-resolvers @custom-resolvers))))

(defn compose-resolver [{:keys [inputs outputs]} fun]
  (let [resolvers (rename-resolvers-that-output outputs)
        renamed (map rename-resolve-out outputs)
        inputs (into inputs renamed)
        fun (fn [_ input]
              (-> input
                  (set/rename-keys (zipmap renamed outputs))
                  fun))]
    (swap! custom-resolvers conj (gen-resolver inputs outputs fun))
    (reset! parser (gen-parser (concat resolvers @custom-resolvers)))))

(defn eql
  "Queries the Pathom graph for the REPLs"
  ([params query] (eql params nil query))
  ([params seed query]
   (let [p (p/deferred)
         params (cond-> params

                        seed
                        (assoc ::pathom/entity (atom seed))

                        (-> params :callbacks nil?)
                        (assoc :callbacks (-> params :editor-state deref :editor/callbacks)))]
     (async/go
       (try
         (let [result (async/<! (@parser params query))
               invalid-val? #{::pathom/reader-error ::pathom/not-found}]
           (p/resolve! p (reduce (fn [sofar [k v]]
                                   (cond-> sofar
                                           (invalid-val? v)
                                           (dissoc k)))
                                 result
                                 result)))
         (catch :default e
           (p/reject! p e))))
     p)))

#_
(eql {:editor-state (:tooling-state @chlorine.state/state)}
     '[{(:repl/namespaces {:filter "repl-tooling.integration."})
        [:repl/namespace {:namespace/vars [:var/fqn]}]}])
