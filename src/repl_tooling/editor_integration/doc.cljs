(ns repl-tooling.editor-integration.doc
  (:require [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.editor-integration.commands :as cmds]
            [repl-tooling.repl-client.clj-helper :refer [contents-for-fn]]
            [promesa.core :as p]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.evaluation :as e-eval]
            [repl-tooling.commands-to-repl.pathom :as pathom]))

(defn- translate-to-doc [meta spec?]
  (str "-------------------------\n"
       (:ns meta) "/" (:name meta) "\n"
       (:arglists meta) "\n  "
       (:doc meta)
       (when (map? spec?)
         (cond-> "\nSpec\n"
                 (:args spec?) (str "  args: " (pr-str (:args spec?)) "\n")
                 (:ret spec?) (str "  ret: " (pr-str (:ret spec?)) "\n")
                 (:fn spec?) (str "  fn: " (pr-str (:fn spec?)))))))

(defn doc-for-var [state]
  (p/let [id (gensym "doc-for-var")
          {:keys [run-feature run-callback]} @state
          seed (run-feature :eql [:editor/data :text/current-var])
          {:editor/keys [current-var data]} seed
          current-range (:text/range current-var (:range data))
          _ (run-callback :on-start-eval {:id id
                                          :editor-data data
                                          :range current-range})
          res (run-feature :eql seed [:render/doc])
          doc (if-let [doc (:render/doc res)]
                {:result (helpers/Interactive. doc)}
                {:error (helpers/LiteralRender. "Can't find doc for this variable")})]
    (run-callback :on-eval {:id id
                            :repl nil
                            :result (assoc doc
                                           :parsed? true
                                           :as-text (-> (:result doc)
                                                        (or (:error doc))
                                                        pr-str
                                                        pr-str))
                            :editor-data data
                            :range current-range})))
