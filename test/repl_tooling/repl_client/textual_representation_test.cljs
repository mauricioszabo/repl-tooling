(ns repl-tooling.repl-client.textual-representation-test
  (:require [clojure.test :refer [async testing is] :include-macros true]
            [check.core :refer-macros [check]]
            [clojure.core.async :as async :include-macros true]
            [repl-tooling.repl-client :as client]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval-helpers :refer-macros [eval-on-repl eval-and-parse]]
            [repl-tooling.editor-integration.renderer :as render]
            [repl-tooling.repl-client.clojure :as clj]))

(set! cards/test-timeout 8000)
(defn as-txt [parsed repl]
  (-> parsed (render/parse-result repl)
      render/txt-for-result))

(cards/deftest evaluate-to-text
  (async done
    (async/go
     (client/disconnect! :clj-ellisions-1)
     (let [repl (clj/repl :clj-ellisions-1 "localhost" 2233 identity)]

       (testing "rendering leafs"
         (check (as-txt (eval-and-parse "10") repl) => [:row [:text "10"]]))

       (testing "rendering big strings"
         (let [after-more (async/promise-chan)
               parsed (render/parse-result (eval-and-parse "(apply str (range 80))") repl)
               [root txt [btn txt2 more-fn] txt3] (render/txt-for-result parsed)]
           (check root => :row)
           (check txt =>
                  [:text (str "\"01234567891011121314151617181920212223242526272829"
                              "303132333435363738394041424344")])
           (check txt2 => "...")
           (check txt3 => [:text "\""])

           (more-fn #(async/put! after-more parsed))
           (check (render/txt-for-result (async/<! after-more)) =>
                  [:row [:text (str "\"01234567891011121314151617181920212223242526272"
                                    "82930313233343536373839404142434445464748495051"
                                    "525354555657585960616263646566676869707172737475"
                                    "76777879\"")]])))
                  ; [:row
                  ;  [:button "..." #()]
                  ;  [:text "\""]])))

       (async/<! (async/timeout 1000))

       (client/disconnect! :clj-ellisions-1)
       (done)))))
