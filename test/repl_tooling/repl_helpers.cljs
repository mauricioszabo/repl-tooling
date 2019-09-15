(ns repl-tooling.repl-helpers
  (:require [clojure.core.async :as async :include-macros true]
            [repl-tooling.integrations.connection :as conn]))

(defn repl-for-cljs! [identifier]
  (let [chan (async/promise-chan)
        upgrade-cmd `(do
                       (~'clojure.core/require '[shadow.cljs.devtools.api])
                       (shadow.cljs.devtools.api/repl :fixture))]
    (.. (conn/connect! identifier "localhost" 2233 upgrade-cmd {:on-result identity
                                                                :on-stdout identity})
        (then #(async/put! chan %)))
    chan))
