(ns repl-tooling.integration
  (:require [etaoin.api :as api]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [shadow.cljs.devtools.api :as shadow]))

(defonce cards (atom nil))

(defn- prepare-selenium! []
  (reset! cards
          (api/boot-driver :chrome {:path-browser "./node_modules/.bin/electron"
                                    :args ["app=./integration.js"]})))

(defn- click-root! []
  (api/click-visible @cards {:css "a:nth-child(1)"})
  (api/wait-has-text @cards {:css "*"} "devcards"))

(defn- click-test! [n]
  (let [ids (api/query-all @cards {:css "a"})
        id (nth ids (inc n))
        [cases txt] (str/split-lines (api/get-element-text-el @cards id))]
    (api/click-el @cards id)
    (println "[TESTSUITE]" txt "- Collecting" cases "tests")))

(defn- collect-tests! [tries]
  (api/wait-exists @cards {:css "div.com-rigsomelight-devcards-panel-heading"})
  (let [ids (api/query-all @cards {:css "div.com-rigsomelight-devcards-panel-heading"})
        fails (atom 0)]
    (doseq [id ids
            :let [description (->> id
                                   (api/get-element-text-el @cards)
                                   str/split-lines
                                   first)
                  xpath (str "//a[contains(.,'" description "')]")]]
      (println "  [TESTCASE]" description)
      (when (api/exists? @cards [xpath "../button"])
        (api/wait-exists @cards [xpath "../button[2]"]))
      (print "    Results: ")
      (let [elements (api/query-all @cards [xpath "../button"])
            results (->> elements
                         (map #(api/get-element-text-el @cards %))
                         (map #(Integer/parseInt %)))
            [total fail-or-pass] results
            style-of-element (when-let [e (second elements)]
                               (api/get-element-attr-el @cards e "style"))]
        (prn results)
        (when (re-find #"rgb\(247, 145, 142\)" (str style-of-element))
          (println "\033[31m      " fail-or-pass "TESTS FAILED ON" description "\033[0m")
          (swap! fails + fail-or-pass)
          (->> [xpath "../..//*[contains(@class, 'com-rigsomelight-devcards-test-line')]"]
               (api/query-all @cards)
               (map #(api/get-element-text-el @cards %))
               (str/join "\n--------------\n")
               println))))
    (if (and (> @fails 0 ) (< tries 5))
      (do
        (println "\n\n        RETRYING..." (inc tries) " time(s)")
        (api/refresh @cards)
        (future (shell/sh "node" "target/fixture.js"))
        (recur (inc tries)))
      @fails)))

(defn run-tests! []
  (prepare-selenium!)
  (click-root!)
  (let [tries (atom 0)]
    (try
      (let [tests (-> @cards (api/query-all {:css "a"}) count dec)
            total-fails (atom 0)]
        (doseq [n (range tests)]
          (click-root!)
          (click-test! n)
          (swap! total-fails + (collect-tests! 0)))

        (println "Total failures:" @total-fails)
        @total-fails)
      (catch Throwable t
        (prn :ERROR t)
        (println "Try number " (inc @tries) "- retrying...\n\n")
        (swap! tries inc)
        (when (< @tries 3)
          (run-tests!)))
      (finally
        (api/close-window @cards)
        (api/quit @cards)))))

(defn run-tests-on-ci {:shadow/requires-server true} []
  (shadow/watch :integration)
  (shadow/watch :fixture)
  (let [sh (future
            (while true
              (shell/sh "node" "target/fixture.js")))
        failures (run-tests!)]
    (System/exit (if (zero? failures) 0 1))))

#_(run-tests!)
