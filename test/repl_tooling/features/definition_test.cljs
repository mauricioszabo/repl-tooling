(ns repl-tooling.features.definition-test
  (:require [clojure.test :refer-macros [testing async is]]
            [devcards.core :as cards :include-macros true]
            [check.core :refer-macros [check]]
            [clojure.core.async :as async :include-macros true]
            [repl-tooling.repl-client :as client]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.repl-client.clojure :as clj]
            [repl-tooling.features.definition :as def])
  (:require-macros [repl-tooling.eval-helpers :refer [eval-on-repl]]))

(set! cards/test-timeout 8000)
(cards/deftest finding-definition
  (async done
    (repls/disconnect! :definition-test1)
    (async/go
     (let [chan (async/promise-chan)
           _ (. (repls/connect-repl! :definition-test1 "localhost" 2233 identity)
               then #(async/put! chan (second %)))
           repl (async/<! chan)
           inside-jar (async/promise-chan)
           in-other-ns (async/promise-chan)
           in-other-by-refer (async/promise-chan)
           in-same-ns (async/promise-chan)
           loading-file (async/promise-chan)]
       (eval-on-repl "(require '[repl-tooling.features.definition-helper :reload :all])")
       (-> repl :session deref :state
           (clj/unrepl-cmd :print-limits
                           {:unrepl.print/string-length 9223372036854775807
                            :unrepl.print/coll-length 9223372036854775807
                            :unrepl.print/nesting-depth 9223372036854775807})
           eval-on-repl)

       (testing "finds symbols inside jars, and get file's contents"
         (. (def/find-var-definition repl 'user "prn") then #(async/put! inside-jar %))
         (-> inside-jar async/<! :line (check => number?))
         (-> inside-jar async/<! :file-name (check => string?))
         (-> inside-jar async/<! :contents (check => string?)))

       (testing "finds symbols inside other namespaces, and gets file"
         (. (def/find-var-definition repl
              'repl-tooling.features.definition-helper "c/some-function")
           then #(do
                   (prn %)
                   (async/put! in-other-ns %)))
         (-> in-other-ns async/<! :line (check => number?))
         (-> in-other-ns async/<! :file-name
             (check => #(re-find #"repl_tooling/features/definition_child\.clj" %)))

         (. (def/find-var-definition repl
              'repl-tooling.features.definition-helper "other-var")
           then #(async/put! in-other-by-refer %))
         (-> in-other-by-refer async/<! :file-name
             (check => #(re-find #"repl_tooling/features/definition_child\.clj" %))))

       (testing "finds symbols inside same namespace, and gets file"
         (. (def/find-var-definition repl
              'repl-tooling.features.definition-helper "some-function")
           then #(async/put! in-same-ns %))
         (-> in-same-ns async/<! :line (check => 3))
         (-> in-same-ns async/<! :file-name
             (check => #(re-find #"repl_tooling/features/definition_helper\.clj" %))))

       (testing "load file, then find symbol"
         (eval-on-repl "(load-file \"test/repl_tooling/features/definition_helper.clj\")")
         (. (def/find-var-definition repl
              'repl-tooling.features.definition-helper "some-function")
           then #(async/put! loading-file %))
         (-> loading-file async/<! :line (check => 3))
         (-> loading-file async/<! :file-name
             (check => #(re-find #"repl_tooling/features/definition_helper\.clj" %))))

       (repls/disconnect! :definition-test1)
       (done)))))
