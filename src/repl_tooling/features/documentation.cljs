(ns repl-tooling.features.documentation
  (:require [clojure-plus.repl :as repl]
            [repl-tooling.editor-helpers :as editor-helpers]))

(defn doc-for [editor row var-name]
  (let [ns-name (repl/ns-for editor)
        var-name (symbol (str "#'" var-name))
        code `(clojure.core/let [m# (clojure.core/meta ~var-name)]
                (new editor-helpers/LiteralRenderResult
                 (clojure.core/str "-------------------------\n"
                                   (:ns m#) "/" (:name m#) "\n"
                                   (:arglists m#) "\n  "
                                   (:doc m#))))]
    (repl/eval-and-present editor ns-name (.getFileName editor) row 0 code)))

(-> js/atom .-workspace .getActiveTextEditor
    (doc-for 11 "+"))
