(ns repl-tooling.editor-integration.autocomplete
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [cljs.core.async :include-macros true :as async]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.evaluation :as evaluation]
            [repl-tooling.features.autocomplete.simple :as simple]
            [repl-tooling.features.autocomplete.compliment :as compliment]))

(defonce clj-autocomplete (atom nil))
(defonce cljs-autocomplete (atom nil))

(defn- detect-clj-compliment [repl]
  (if-let [kind @clj-autocomplete]
    kind
    (p/let [res (.. (eval/eval repl "(clojure.core/require 'compliment.core)")
                    (then (constantly :compliment))
                    (catch (constantly :simple)))]
      (reset! clj-autocomplete res))))

(defn- detect-cljs-compliment [repl]
  (if repl
    (if-let [kind @cljs-autocomplete]
      kind
      (.. (eval/eval repl "(clojure.core/require 'compliment.sources.cljs)")
          (then (constantly :compliment))
          (catch (constantly :simple))))
    :simple))

(def ^:private non-clj-var-regex #"[^a-zA-Z0-9\-.$!?\/><*=\?_:]+")
(defn- get-prefix [code row col]
  (-> code
      str/split-lines
      (nth row "")
      (->> (take col)
           (apply str))
      (str/split non-clj-var-regex)
      last
      str))

(defn- autocomplete-clj [repl kind {:keys [contents range]}]
  (let [position (first range)
        [orig-row orig-col] position
        [[[block-row block-col]] block-text] (helpers/top-block-for contents position)
        prefix (get-prefix contents orig-row orig-col)
        ns-name (-> contents
                    (helpers/ns-range-for position)
                    second)
        [row col] (if block-text
                    [(- orig-row block-row) orig-col]
                    [0 0])]
    (if (= :compliment kind)
      (compliment/for-clojure repl ns-name (str block-text) prefix row col)
      (simple/for-clj repl ns-name prefix))))

(defn- autocomplete-cljs [clj-repl cljs-repl kind cmd {:keys [contents range]}]
  (let [position (first range)
        [orig-row orig-col] position
        [[[block-row block-col]] block-text] (helpers/top-block-for contents position)
        prefix (get-prefix contents orig-row orig-col)
        ns-name (-> contents
                    (helpers/ns-range-for position)
                    second)
        [row col] (if block-text
                    [(- orig-row block-row) orig-col]
                    [0 0])]
    (if (= :compliment kind)
      (compliment/for-cljs clj-repl cmd ns-name (str block-text) prefix row col)
      (or (some-> cljs-repl (simple/for-cljs ns-name prefix))
          []))))

(defn- resolve-clj [state opts editor-data]
  (if-let [aux-repl (:clj/aux @state)]
    (p/let [kind (detect-clj-compliment aux-repl)]
      (autocomplete-clj (:clj/aux @state) kind editor-data))
    (p/promise [])))

(defn- resolve-cljs [state opts editor-data]
  (p/let [kind (detect-cljs-compliment (:clj/aux @state))]
    (autocomplete-cljs (:clj/aux @state)
                       (:cljs/repl @state)
                       kind
                       (-> @state :repl/info :cljs/repl-env)
                       editor-data)))

(defn command [state {:keys [get-config] :as opts} editor-data]
  (if (evaluation/need-cljs? (get-config) (:filename editor-data))
    (resolve-cljs state opts editor-data)
    (resolve-clj  state opts editor-data)))
