(ns repl-tooling.editor-integration.doc
  (:require [repl-tooling.editor-helpers :as helpers]
            [clojure.core.async :as async]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.evaluation :as e-eval]))

(defn- resolved-var [var-name namespace repl]
  (let [c (async/promise-chan)]
    (eval/evaluate repl
                     (str "`" var-name)
                     {:namespace namespace}
                     #(async/put! c %))
    c))
#_
(defn doc-for [editor ^js range str-var-name]
  (let [ns-name (repl/ns-for editor)
        var-name (symbol (str "(clojure.core/resolve '" str-var-name ")"))
        in-result (inline/new-result editor (.. range -end -row))
        code `(~'clojure.core/let [v# (~'clojure.core/or ~var-name
                                                     (throw
                                                       (~'clojure.core/ex-info
                                                        (~'clojure.core/str "Unable to resolve var: " ~str-var-name
                                                                            " in this context in file " ~(.getFileName editor))
                                                        {})))
                                   m# (~'clojure.core/meta v#)]
                (~'clojure.core/str "-------------------------\n"
                                    (:ns m#) "/" (:name m#) "\n"
                                    (:arglists m#) "\n  "
                                    (:doc m#)))]
    (repl/evaluate-aux editor
                       ns-name
                       (.getFileName editor)
                       (.. range -start -row)
                       (.. range -start -column)
                       code
                       {:literal true :ignore true}
                       #(inline/render-inline! in-result %))))

(defn doc-for-var [{:keys [contents range filename]} opts state]
  (async/go
   (let [[_ var] (helpers/current-var contents (first range))
         [_ ns] (helpers/ns-range-for contents (first range))
         repl (e-eval/repl-for opts state filename)
         var (-> var (resolved-var ns repl) async/<! delay)]
     (prn :Va @var))))
  ; (let [editor ^js (atom/current-editor)
  ;       pos (.getCursorBufferPosition editor)]
  ;   (doc-for editor #js {:start pos :end pos} (atom/current-var editor))))
