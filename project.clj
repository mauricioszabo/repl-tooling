(defproject repl-tooling "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojurescript "1.10.339"]
                 [cljs-node-io "0.5.0"]
                 [org.clojure/core.async "0.4.474"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.16"]]

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [check "0.0.1-SNAPSHOT"]
                                  ; [com.bhauman/figwheel-main "0.1.4"]
                                  ; [cider/piggieback "0.3.6"]
                                  ; [com.bhauman/rebel-readline-cljs "0.1.4"]

                                  [com.cemerick/piggieback "0.2.2"]
                                  [figwheel-sidecar "0.5.16"]]}}

  :source-paths ["lib" "src"]
  ; :clean-targets ^{:protect false} ["lib/js"]
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src" "test"]
                        :figwheel true
                        :compiler {:main repl-tooling.all-tests
                                   ; :output-to "lib/js/main.js"
                                   ; :output-dir "lib/js"
                                   :target :nodejs
                                   :optimizations :none
                                   :source-map true
                                   :warnings {:single-segment-namespace false}}}]}

                       ; {:id "release"
                       ;  :source-paths ["src"]
                       ;  :figwheel true
                       ;  :compiler {:main repl-tooling.core
                       ;             :output-to "lib/js/main.js"
                       ;            ;  :output-dir "lib/js"
                       ;             :target :nodejs
                       ;             :optimizations :simple
                       ;             :output-wrapper true}}]}
  :figwheel {})
