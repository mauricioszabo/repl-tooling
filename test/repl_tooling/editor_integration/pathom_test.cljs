(ns repl-tooling.editor-integration.pathom-test)
;   (:require [devcards.core :as cards]
;             [repl-tooling.commands-to-repl.pathom :as pathom]
;             [matcher-combinators.matchers :as m]
;             [clojure.test]
;             [promesa.core :as p]
;             [repl-tooling.integration.fake-editor :as fake]
;             [check.async :refer [async-test check testing]]
;             [repl-tooling.repl-client.clojure :as clj]
;             [repl-tooling.repl-client.shadow-ws :as shadow-ws]))
;
; (def config (atom {:eval-mode :prefer-clj
;                    :project-paths [(. js/process cwd)]}))
;
; (def clj-repls {:repl/eval #(instance? clj/Evaluator %)
;                 :repl/aux #(instance? clj/Evaluator %)
;                 :repl/clj #(instance? clj/Evaluator %)})
; (def cljs-repls {:repl/eval #(instance? shadow-ws/ShadowCLJS %)
;                  :repl/aux #(instance? shadow-ws/ShadowCLJS %)
;                  :repl/clj #(instance? clj/Evaluator %)})
;
; (cards/deftest pathom-resolver-with-repl
;   (async-test "pathom resolvers" {:teardown (fake/disconnect!)
;                                   :timeout 16000}
;     (fake/connect! {:get-config #(deref config)})
;
;     (testing "resolves editor data"
;       (swap! fake/state assoc :filename "somefile.cljc" :range [[0 0] [0 0]])
;       (fake/type "(ns foo)\n\n(str 1 2)")
;       (check (pathom/eql {:editor-state (-> @fake/state :editor-state)}
;                          [:editor/contents :editor/filename :editor/range])
;              => {:editor/contents "(ns foo)\n\n(str 1 2)"
;                  :editor/filename "somefile.cljc"
;                  :editor/range [[0 0] [0 0]]}))
;
;     (testing "derives information from the editor data"
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (check (pathom/eql {:editor-state (-> @fake/state :editor-state)}
;                          [:editor/namespace :editor/ns-range
;                           :editor/current-var :editor/current-var-range])
;              => {:editor/namespace "foo"
;                  :editor/ns-range [[0 0] [0 7]]
;                  :editor/current-var "str"
;                  :editor/current-var-range [[2 1] [2 3]]}))
;
;     (testing "will get Clojure REPLs info from config only"
;       (swap! config assoc :eval-mode :clj)
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          [:repl/eval :repl/aux :repl/clj])
;              => clj-repls))
;
;     (testing "will get ClojureScript REPLs info from config only"
;       (fake/run-command! :connect-embedded)
;       (swap! config assoc :eval-mode :cljs)
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          [:repl/eval :repl/aux :repl/clj])
;              => cljs-repls))
;
;     (testing "will get Clojure REPL info from config + editor data"
;       (swap! fake/state assoc :filename "somefile.cljc")
;       (swap! config assoc :eval-mode :prefer-clj)
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          [:repl/eval :repl/aux :repl/clj])
;              => clj-repls)
;
;       (swap! config assoc :eval-mode :prefer-cljs)
;       (swap! fake/state assoc :filename "somefile.clj")
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          [:repl/eval :repl/aux :repl/clj])
;              => clj-repls))
;
;     (testing "will get ClojureScript REPL info from config + editor data"
;       (swap! fake/state assoc :filename "somefile.cljc")
;       (swap! config assoc :eval-mode :prefer-cljs)
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          [:repl/eval :repl/aux :repl/clj])
;              => cljs-repls)
;
;       (swap! config assoc :eval-mode :prefer-clj)
;       (swap! fake/state assoc :filename "somefile.cljs")
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          [:repl/eval :repl/aux :repl/clj])
;              => cljs-repls)
;
;       (swap! config assoc :eval-mode :prefer-cljs)
;       (swap! fake/state assoc :filename "somefile.cljs")
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          [:repl/eval :repl/aux :repl/clj])
;              => cljs-repls))
;
;     (testing "getting current namespace when there's a NS form"
;       (swap! config assoc :eval-mode :clj)
;       (fake/type "(ns foo.bar) (+ 1 2)")
;       (swap! fake/state assoc :range [[0 2] [0 2]])
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)} [:repl/namespace])
;              => {:repl/namespace 'foo.bar}))
;
;     (testing "getting current namespace when there's no NS form"
;       (swap! config assoc :eval-mode :clj)
;       (fake/type "(+ 1 2)")
;       (swap! fake/state assoc :range [[0 2] [0 2]])
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)} [:repl/namespace])
;              => {:repl/namespace 'user})
;
;       (swap! config assoc :eval-mode :cljs))
;     (check (pathom/eql {:editor-state (:editor-state @fake/state)} [:repl/namespace])
;            => {:repl/namespace 'cljs.user})
;
;     (testing "full qualified name variables"
;       (fake/type "(ns foo)\n\n(str 1 2)")
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (swap! config assoc :eval-mode :clj)
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)} [:var/fqn])
;              => {:var/fqn 'clojure.core/str})
;
;       (swap! config assoc :eval-mode :cljs)
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)} [:var/fqn])
;              => {:var/fqn 'cljs.core/str}))
;
;     (testing "getting meta from Clojure vars"
;       (swap! config assoc :eval-mode :clj)
;       (swap! fake/state assoc
;              :range [[2 1] [2 1]] :code "(ns promesa.core)\n\n(str 1 2)")
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)} [:var/meta])
;              => {:var/meta {:doc #"With no args, returns the empty string"}}))
;
;     (testing "getting meta from ClojureScript vars"
;       (swap! config assoc :eval-mode :cljs)
;       (fake/type "(ns repl-tooling.integration.fixture-app)\n\n(p/deferred 1 2)")
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)} [:var/meta])
;              => {:var/meta {:doc #"Creates an empty promise"}}))
;
;     (testing "getting meta from ClojureScript macros"
;       (swap! config assoc :eval-mode :cljs)
;       (fake/type "(ns repl-tooling.integration.fixture-app)\n\n(p/let [] )")
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)} [:var/meta])
;              => {:var/meta {:doc #"always returns promise"}}))
;
;     (testing "getting spec of vars"
;       (swap! config assoc :eval-mode :clj)
;       (fake/type "(ns user)\n\n(let [a 1])")
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)} [:var/spec])
;              => {:var/spec '{:args (cat :bindings :clojure.core.specs.alpha/bindings
;                                         :body (* any?))
;                              :ret any?}}))
;
;     (testing "getting var definition from core locations"
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          [:definition/filename :definition/row :definition/file-contents])
;              => {:definition/filename #"clojure.*jar!/clojure/core.clj"
;                  :definition/row number?
;                  :definition/file-contents string?}))
;
;     (testing "getting var definition from local locations"
;       (swap! config assoc :eval-mode :cljs)
;       (fake/type "(ns repl-tooling.editor-integration.connection)\n\n(connect! [] )")
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          [:definition/filename :definition/row])
;              => {:definition/filename #"editor_integration/connection.cljs"
;                  :definition/row 215}))
;
;     (testing "getting full qualified vars in all namespaces"
;       (swap! config assoc :eval-mode :cljs)
;       (check (pathom/eql {:editor-state (:editor-state @fake/state)}
;                          '[{(:repl/namespaces {:filter "repl-tooling.integration."})
;                             [:repl/namespace {:namespace/vars [:var/fqn]}]}])
;              => {:repl/namespaces
;                  (m/embeds
;                   [{:repl/namespace 'repl-tooling.integration.fixture-app
;                     :namespace/vars
;                     (m/in-any-order
;                      [{:var/fqn 'repl-tooling.integration.fixture-app/private-var}
;                       {:var/fqn 'repl-tooling.integration.fixture-app/local-var}
;                       {:var/fqn 'repl-tooling.integration.fixture-app/private-fn}
;                       {:var/fqn 'repl-tooling.integration.fixture-app/local-fn}
;                       {:var/fqn 'repl-tooling.integration.fixture-app/some-replace}
;                       {:var/fqn 'repl-tooling.integration.fixture-app/main}])}])}))))
;
; (def callbacks
;   {:on-disconnect identity
;    :on-stdout identity
;    :on-eval identity
;    :notify identity
;    :prompt identity
;    :get-config (constantly {:eval-mode :prefer-clj
;                             :project-paths [(. js/process cwd)]})
;    :on-stderr identity
;    :editor-data #(let [code (:code @fake/state)]
;                    {:contents code
;                     :filename (:filename @fake/state)
;                     :range (:range @fake/state)})})
;
; (cards/deftest pathom-resolver-without-repl
;   (async-test "resolving with clj-kondo" {:timeout 18000
;                                           :teardown (pathom/reset-resolvers)}
;     (testing "will get fqn from aliases"
;       (fake/type "(ns repl-tooling.editor-integration.connection)\n\n(p/let [] )")
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (check (pathom/eql {:callbacks callbacks} [:var/fqn])
;              => {:var/fqn 'promesa.core/let}))
;
;     (testing "will get fqn from refers"
;       (fake/type "(ns repl-tooling.integration.fixture-app)\n\n(replace-first )")
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (check (pathom/eql {:callbacks callbacks} [:var/fqn])
;              => {:var/fqn 'clojure.string/replace-first}))
;
;     (testing "will get fqn from definitions in the same NS"
;       (fake/type "(ns repl-tooling.integration.fixture-app)\n\n(private-fn )")
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (check (pathom/eql {:callbacks callbacks} [:var/fqn])
;              => {:var/fqn 'repl-tooling.integration.fixture-app/private-fn}))
;
;     (testing "will get meta from Kondo's result"
;       (fake/type "(ns repl-tooling.editor-integration.connection)\n\n(connect! [] )")
;       (swap! fake/state assoc :range [[2 1] [2 1]])
;       (check (pathom/eql {:callbacks callbacks} [:var/meta])
;              => {:var/meta {:doc #"Connects to a clojure.*REPL"}}))
;
;     (testing "customizing resolves"
;       (testing "will add a new resolver with our code"
;         (pathom/add-resolver {:outputs [:var/meta] :inputs [:editor/current-var]}
;                              (fn [{:editor/keys [current-var]}]
;                                {:var/meta {:doc current-var}}))
;         (fake/type "(ns repl-tooling.editor-integration.connection)\n\n(lol! [] )")
;         (swap! fake/state assoc :range [[2 1] [2 1]])
;         (check (pathom/eql {:callbacks callbacks} [:var/meta])
;                => {:var/meta {:doc "lol!"}}))
;
;       (testing "will compose original resolver, and add our customization code"
;         (pathom/reset-resolvers)
;         (pathom/compose-resolver {:outputs [:var/meta] :inputs [:var/fqn]}
;                                  (fn [{:var/keys [fqn meta]}]
;                                    {:var/meta (assoc meta :original-var fqn)}))
;         (fake/type "(ns repl-tooling.editor-integration.connection)\n\n(connect! [] )")
;         (swap! fake/state assoc :range [[2 1] [2 1]])
;         (check (pathom/eql {:callbacks callbacks} [:var/meta])
;                => {:var/meta {:doc #"Connects to a clojure.*REPL"
;                               :original-var 'repl-tooling.editor-integration.connection/connect!}})))))
;
; (cards/defcard-rg fake-editor
;   fake/editor
;   fake/state)
