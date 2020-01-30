(ns repl-tooling.features.shadow-cljs
  (:require [cljs.reader :as edn]
            ["path" :as path]
            ["fs" :refer [existsSync readFileSync]]
            [repl-tooling.integrations.repls :as repls]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj-repl]
            [repl-tooling.repl-client.source :as source]
            [promesa.core :as p]))

(defn- readfile [shadow-path]
  (-> shadow-path readFileSync str edn/read-string
      :builds keys))

(defn cmd-for [build-id]
  `(do
     (~'clojure.core/require '[shadow.cljs.devtools.api])
     (shadow.cljs.devtools.api/repl ~build-id)))

(defn- cmds-for [shadow-path]
  (->> (readfile shadow-path)
       (map (juxt identity cmd-for))
       (into {})))

(defn command-for [project-paths]
  (let [first-shadow-file (->> project-paths
                               (map #(path/join % "shadow-cljs.edn"))
                               (filter existsSync)
                               first)]
    (if first-shadow-file
      (cmds-for first-shadow-file)
      {:error :no-shadow-file})))

(defn commands-for [repl]
  (let [cmd "(do
                (clojure.core/require 'shadow.cljs.devtools.api)
                (clojure.core/require 'shadow.cljs.devtools.server.worker))
                (clojure.core/filter shadow.cljs.devtools.api/worker-running?
                                     (shadow.cljs.devtools.api/get-build-ids)))"]
    (.. (eval/eval repl cmd)
        (then #(if (not-empty %)
                 (->> %
                       :result
                       (map (juxt identity cmd-for))
                       (into {}))
                 {:error :workers-empty}))
        (catch #(hash-map :error :no-shadow)))))

(defrecord Shadow [clj-evaluator build-id]
  eval/Evaluator
  (evaluate [self command opts callback]
    (let [id (or (:id opts) (gensym))
          clj-opts (dissoc opts :namespace)
          name-space (:namespace opts)
          code (source/wrap-command id command ":default" false)
          clj-cmd (str "(clojure.core/->
                          (shadow.cljs.devtools.server.worker/worker-request
                            (shadow.cljs.devtools.api/get-worker " build-id ")
                            {:type :repl-eval :input " (pr-str (str command)) "})
                          :results
                          clojure.core/last
)")]

      (println :CODE "\n" clj-cmd)
      (if (:error code)
        (let [output (-> clj-evaluator :state :on-output)]
          (output code)
          (callback code))
        (eval/evaluate clj-evaluator clj-cmd clj-opts callback))
        ; (eval-code {:evaluator self :id id :callback callback :conn conn :code code}
        ;            opts))
      id))

  (break [this repl]))

(defn upgrade-repl [repl command]
  (clj-repl/disable-limits! repl))

#_
(let [repl (-> @chlorine.state/state :tooling-state deref :clj/aux)
      shadow (->Shadow repl :dev)]
  (.. (eval/eval shadow "#'async/lol")
      (then prn)
      (catch #(prn :ERROR %))))

#_
(let [repl (-> @chlorine.state/state :tooling-state deref :clj/aux)
      shadow (->Shadow repl :dev)]
  (.. (eval/eval shadow "(throw (ex-info \"lol\" {}))")
      (then prn)
      (catch prn)))
