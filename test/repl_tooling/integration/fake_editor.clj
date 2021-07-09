(ns repl-tooling.integration.fake-editor)

(defmacro changing-result [binding & body]
  `(let [~binding (change-result-p)]
     ~@body
     ~binding))

(defmacro changing-stdout [binding & body]
  `(let [~binding (change-stdout-p)]
     ~@body
     ~binding))
