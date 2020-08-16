(ns repl-tooling.integration.fake-editor)

(defmacro changing-result [binding & body]
  `(let [~binding (change-result)]
     ~@body))

(defmacro changing-stdout [binding & body]
  `(let [~binding (change-stdout)]
     ~@body))
