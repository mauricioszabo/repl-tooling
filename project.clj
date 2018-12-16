(defproject repl-tooling "0.0.1"
  :dependencies [[org.clojure/clojurescript "1.10.339"]
                 [org.clojure/core.async "0.4.474"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.16"]]

  :jvm-opts ["-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [check "0.0.2-SNAPSHOT"]
                                  [thheller/shadow-cljs "2.6.6"]]
                   :source-paths ["src" "test"]}}

  :source-paths ["src"])
