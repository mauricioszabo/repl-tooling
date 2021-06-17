(ns repl-tooling.editor-integration.pathom-test
  (:require [devcards.core :as cards :include-macros true]
            [repl-tooling.commands-to-repl.pathom :as pathom]
            [matcher-combinators.matchers :as m]
            [clojure.test]
            [promesa.core :as p]
            [repl-tooling.integration.fake-editor :as fake]
            [check.async :refer [async-test check testing]]
            [repl-tooling.repl-client.clojure :as clj]
            [repl-tooling.repl-client.shadow-ws :as shadow-ws]))

(def config (atom {:eval-mode :prefer-clj
                   :project-paths [(. js/process cwd)]}))

(cards/deftest customizing-pathom
  (async-test "pathom resolver customization" {:teardown (fake/disconnect!)
                                               :timeout 16000}
    (fake/connect! {:get-config #(deref config)})

    (testing "customizing resolves"
      (testing "will add a new resolver with our code"
        (fake/type "some-symbol")
        (pathom/add-resolver {:outputs [:var/meta] :inputs [:text/current-var]}
                             (fn [{:text/keys [current-var]}]
                               {:var/meta {:doc (:text/contents current-var)}}))
        (check (fake/run-feature! :eql [:var/meta])
               => {:var/meta {:doc "some-symbol"}}))

      (testing "will compose original resolver, and add our customization code"
        (fake/type "str")
        (pathom/reset-resolvers)
        (pathom/compose-resolver {:outputs [:var/meta] :inputs [:var/fqn]}
                                 (fn [{:var/keys [fqn meta]}]
                                   {:var/meta (assoc meta :original-var fqn)}))
        (check (fake/run-feature! :eql [:var/meta])
               => {:var/meta {:doc #"concatenation"
                              :original-var 'clojure.core/str}})))))
