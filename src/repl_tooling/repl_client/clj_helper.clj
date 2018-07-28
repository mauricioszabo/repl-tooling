(ns repl-tooling.repl-client.clj-helper)

(defmacro blob-contents []
  (slurp "unrepl.clj"))
