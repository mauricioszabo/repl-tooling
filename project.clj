(defproject repl-tooling "0.3.0"
  :dependencies [[org.clojure/core.async "0.4.490"]]

  :source-paths ["src"]

  :plugins [[lein-cljsbuild "1.1.7"]
            [reagent "0.8.1"]
            [rewrite-cljs "0.4.4"]])

  ; :profiles {:dev {:dependencies []
  ;                  :source-paths ["src" "test"]}})
