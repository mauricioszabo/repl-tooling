(ns repl-tooling.editor-integration.stacktraces-test
  (:require [devcards.core :as cards :include-macros true]
            [clojure.test]
            [promesa.core :as p]
            [repl-tooling.integration.fake-editor :as fake]
            [check.async :refer [async-test check testing]]))

(defn- click-link [link-text]
  (p/let [find-link
          (fn []
            (->> "div.result a"
                 (.querySelectorAll js/document)
                 rest
                 (filter #(->> % .-innerText (re-find (re-pattern link-text))))
                 first))

          link (fake/wait-for-p find-link)]
    (.click link)))

(defn open-editor-callback []
  (let [last-call (atom (p/deferred))]
    {:opened-editor (fn [] (let [prom @last-call]
                             (p/then prom #(reset! last-call (p/deferred)))
                             prom))
     :callback (fn [res]
                 (p/resolve! @last-call res))}))

(cards/deftest clickable-stacktraces
  (let [{:keys [opened-editor callback]} (open-editor-callback)]
    (async-test "pathom resolvers" {:teardown (fake/disconnect!)
                                    :timeout 8000}
      (fake/connect! {:open-editor callback})

      (testing "will open a Clojure code from a stacktrace derived from classpath"
        (swap! fake/state assoc :filename "test/repl_tooling/integration/fake_editor.clj")
        (fake/type-and-eval "(/ 10 0)")
        (click-link "fake_editor.clj:1")
        (check (opened-editor)
               => {:file-name "test/repl_tooling/integration/fake_editor.clj"
                   :line 0}))

      (testing "will open a Clojure source code inside a JAR"
        (click-link "core.clj")
        (check (opened-editor)
               => {:file-name #"clojure.*jar!.*core.clj"
                   :contents #"clojure.core"}))

      ; FIXME: Stacktraces are not working on Electron. Don't know why
      #_
      (testing "will parse source maps for ClojureScript"
        (fake/run-command! :connect-embedded)
        (p/delay 200)
        (swap! fake/state assoc :filename "foo.cljs")
        (fake/type-and-eval "(repl-tooling.integration.fixture-app/some-replace :what)")
        (click-link "fixture_app.cljs")
        (check (opened-editor)
               => {:file-name #"test/repl_tooling/integration/fixture_app\.cljs"})))))

(cards/defcard-rg fake-editor
  fake/editor
  fake/state)
