(ns repl-tooling.features.autocomplete.compliment
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [repl-tooling.eval :as eval]))

(defn- re-escape [str]
  (str/replace str #"[.*+?^${}()|\[\]\\]" "\\$&"))

(defn- make-context [text prefix row col]
  (let [lines (str/split-lines text)
        pattern (re-pattern (str "(.{" (- col (count prefix)) "})" (re-escape prefix)))]
    (->> "$1__prefix__"
         (update lines row str/replace-first pattern)
         (str/join "\n"))))

(defn for-clojure
  ([repl ns-name text prefix row col]
   (for-clojure repl ns-name text prefix row col nil))
  ([repl ns-name text prefix row col sources]
   (let [ns (when ns-name (symbol ns-name))
         context (make-context text prefix row col)
         code `(do
                  (~'clojure.core/require '[compliment.core])
                  (~'clojure.core/let [completions# (compliment.core/completions
                                                     ~prefix
                                                     {:tag-candidates true
                                                      :ns '~ns
                                                      :sources ~sources
                                                      :context ~context})]
                    (~'clojure.core/vec completions#)))]
     (.. (eval/eval repl code)
         (then #(:result %))
         (catch (constantly []))))))

(defn for-cljs [repl cmd-for-cljs-env ns-name text prefix row col]
  (p/let [ns (when ns-name (symbol ns-name))
          context (make-context text prefix row col)
          code `(do
                 (~'clojure.core/require 'compliment.sources.cljs)
                 (~'clojure.core/binding [compliment.sources.cljs/*compiler-env*
                                          ~cmd-for-cljs-env]
                   (compliment.sources.cljs/candidates ~prefix
                                                       '~ns
                                                       ~context)))
           {:keys [result]} (.catch (eval/eval repl code) (constantly {:result []}))
           clj-result (for-clojure repl ns-name text prefix row col
                                   [:compliment.sources.local-bindings/local-bindings
                                    :compliment.sources.keywords/keywords])]
    (->> result
         (concat clj-result)
         distinct
         (sort-by :candidate))))
