(ns repl-tooling.editor-integration.doc
  (:require [repl-tooling.editor-helpers :as helpers]
            [promesa.core :as p]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.evaluation :as e-eval]))

(defn- doc-cmd [var filename]
  `(~'clojure.core/let
     [v# (~'clojure.core/or (~'clojure.core/resolve '~var)
                            (throw
                              (~'clojure.core/ex-info
                                (~'clojure.core/str
                                  "Unable to resolve var: '" '~var "' in this context in file ")
                               {:var '~var :filename ~filename})))
      m# (~'clojure.core/meta v#)]
     (~'clojure.core/str "-------------------------\n"
                         (:ns m#) "/" (:name m#) "\n"
                         (:arglists m#) "\n  "
                         (:doc m#))))

(defn- spec-cmd [var]
  (str "
    (clojure.core/when-let [s (clojure.spec.alpha/get-spec '" var ")]
      (clojure.core/some->> [:args :ret :fn]
        (clojure.core/map #(clojure.core/some->> (% s)
                              clojure.spec.alpha/describe
                              clojure.core/pr-str
                              (clojure.core/str (clojure.core/name %)\": \")))
        (clojure.core/remove clojure.core/nil?)
        clojure.core/not-empty
        (clojure.core/interpose \"\\n\")
        (clojure.core/apply str)))"))

(defn- emit-result [document-part spec-part {:keys [opts eval-data]}]
  (let [docs (cond-> document-part spec-part (str "\n\nSpec:\n" spec-part))
        {:keys [on-eval on-result]} opts
        res (helpers/parse-result {:result (pr-str docs) :literal true :as-text (pr-str docs)})]

    (and on-eval (on-eval (assoc eval-data :result res)))
    (and on-result (on-result res))))

(defn- try-spec [document-part options]
  (let [spec-ed (p/let [_ (eval/eval (:repl options)
                                     "(clojure.core/require '[clojure.spec.alpha])")
                        cmd (spec-cmd (:var options))]
                  (eval/eval (:repl options) cmd))]
    (.. spec-ed
        (then #(emit-result document-part % options))
        (catch #(emit-result document-part nil options)))))

(defn- treat-error [error {:keys [opts eval-data]}]
  (let [{:keys [on-eval on-result]} opts
        result {:error error :parsed? true :as-text (pr-str error)}]
    (prn :RES error)
    (and on-eval (on-eval (assoc eval-data :result result)))
    (and on-result (on-result result))))

(defn- run-documentation-code [{:keys [var editor-data opts repl] :as options}]
  (let [on-start (-> options :opts :on-start-eval)]
    (on-start (:eval-data options)))
  (p/catch (p/let [var (eval/eval repl (str "`" var) {:namespace (:ns options) :ignore true})
                   document-part (eval/eval repl (doc-cmd var (:filename editor-data)))]
              (if document-part
                (try-spec document-part (assoc options :var var))
                (treat-error "\"Unknown error\"" options)))
           #(treat-error % options)))

(defn doc-for-var [{:keys [contents range filename] :as editor-data} opts state]
  (let [id (gensym "doc-for-var")
        [_ var] (helpers/current-var contents (first range))
        [_ ns] (helpers/ns-range-for contents (first range))
        repl (e-eval/repl-for opts state filename true)
        eval-data {:id id
                   :editor-data editor-data
                   :range range}]
    (when repl
      (run-documentation-code {:ns ns
                               :repl repl
                               :var var
                               :opts opts
                               :eval-data eval-data
                               :editor-data editor-data}))))
