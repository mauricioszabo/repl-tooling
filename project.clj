(defproject repl-tooling "0.0.1"
  :dependencies [[org.clojure/clojurescript "1.10.520"]
                 [org.clojure/core.async "0.4.490"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.16"]]

  :jvm-opts ["-Dclojure.server.repl={:port 2233 :accept clojure.core.server/repl}"]

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.0"]
                                  [check "0.0.3-SNAPSHOT"]
                                  [devcards "0.2.5"]
                                  [reagent "0.8.1"]
                                  [thheller/shadow-cljs "2.7.36"]]
                   :source-paths ["src" "test"]}}

  :source-paths ["src"])
