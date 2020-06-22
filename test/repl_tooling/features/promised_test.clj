(ns repl-tooling.features.promised-test
  (:require [promesa.core :as p]
            [clojure.test :as t]))

(defmacro promised-test [opts & body]
  (let [timeout-k (-> (gensym "--TIMEOUT-") str keyword)
        timeout-t (:timeout opts 3000)
        timeout-prom `(p/delay ~timeout-t ~timeout-k)
        test-prom `(p/do! ~@body)]
    `(t/async done#
       (-> [~test-prom ~timeout-prom]
           (p/race)
           (p/finally (fn [res# err#]
                        ~(:teardown opts)
                        (cond
                          err#
                          (t/is (not (ex-info "Error on test result" {:error err#})))

                          (= res# ~timeout-k)
                          (t/is (not (ex-info "Async error - not finalized" {}))))
                        (done#)))))))

(defmacro testing [description & body]
  `(t/testing ~description
     (p/do! ~@body)))
