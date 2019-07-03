(ns repl-tooling.editor-integration.evaluation
  (:require [clojure.string :as str]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]))

(defn need-cljs? [config filename]
  (or
   (-> config :eval-mode (= :cljs))
   (and (-> config :eval-mode (= :discover))
        (str/ends-with? (str filename) ".cljs"))
   (and (-> config :eval-mode (= :prefer-cljs))
        (or (str/ends-with? (str filename) ".cljs")
            (str/ends-with? (str filename) ".cljc")
            (str/ends-with? (str filename) ".cljx")))))

(defn- treat-error [notify! cljs? clj-connected?]
  (let [msg (cond
              (and cljs? clj-connected?)
              (str "REPL not connected for ClojureScript.\n\n"
                   "You can connect a REPL using 'Connect ClojureScript Socket REPL' "
                   "command, or 'Connect Embedded ClojureScript' command")

              cljs?
              (str "REPL not connected for ClojureScript.\n\n"
                   "You can connect a REPL using 'Connect ClojureScript Socket REPL' "
                   "command, or by connecting a Clojure REPL and running "
                   "'Connect embedded ClojureScript' command")

              :clj
              (str "REPL not connected for Clojure\n\n"
                   "You can connect a REPL by running 'Connect Clojure Socket REPL' command"))]
    (notify! {:type :error
              :title "REPL not connected"
              :message msg})))

(defn eval-cmd [state code namespace range editor-data opts]
  (when code
    (let [id (atom nil)
          filename (:filename editor-data)
          {:keys [on-start-eval on-eval]} opts
          [[row col]] range
          cljs? (need-cljs? ((:get-config opts)) filename)
          repl (if cljs? (:cljs/repl @state) (:clj/repl @state))
          eval-data (delay {:id @id
                            :editor-data editor-data
                            :range range})]

      (if (nil? repl)
        (treat-error (:notify opts) cljs? (:clj/repl @state))
        (do
          (reset! id (eval/evaluate repl
                                    code
                                    {:filename filename
                                     :row (inc row)
                                     :col (inc col)
                                     :namespace (str namespace)}
                                    #(and on-eval
                                          (on-eval (assoc @eval-data
                                                          :result (helpers/parse-result %))))))
          (and on-start-eval (on-start-eval @eval-data)))))))
