(ns repl-tooling.editor-integration.autocomplete
  (:require [clojure.string :as str]
            [cljs.core.async :include-macros true :as async]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-integration.evaluation :as evaluation]
            [repl-tooling.features.autocomplete.simple :as simple]
            [repl-tooling.features.autocomplete.compliment :as compliment]))

(defonce clj-autocomplete (atom nil))
(defonce cljs-autocomplete (atom nil))

(defn- detect-clj-compliment [repl]
  (let [chan (async/promise-chan)]
    (eval/evaluate repl "(clojure.core/require 'compliment.core)"
                   {:ignore true}
                   #(async/put! chan (contains? % :result)))
    chan))

(defn- detect-cljs-compliment [repl]
  (let [chan (async/promise-chan)]
    (eval/evaluate repl "(clojure.core/require 'compliment.sources.cljs)"
                   {:ignore true}
                   #(async/put! chan (contains? % :result)))
    chan))

(def ^:private non-clj-var-regex #"[^a-zA-Z0-9\-.$!?\/><*=\?_:]+")
(defn- get-prefix [code row col]
  (-> code
      str/split-lines
      (nth row)
      (->> (take col)
           (apply str))
      (str/split non-clj-var-regex)
      last))

(defn- autocomplete-clj [repl {:keys [contents range]}]
  (let [position (first range)
        [orig-row orig-col] position
        [[[block-row block-col]] block-text] (helpers/top-block-for contents position)
        prefix (get-prefix contents orig-row orig-col)
        ns-name (-> contents
                    (helpers/ns-range-for position)
                    second)
        [row col] (if range
                    [(- orig-row block-row) orig-col]
                    [0 0])]
    (if (= :compliment @clj-autocomplete)
      (compliment/for-clojure repl ns-name block-text prefix row col)
      (simple/for-clj repl ns-name prefix))))

(defn- autocomplete-cljs [clj-repl cljs-repl cmd {:keys [contents range]}]
  (let [position (first range)
        [orig-row orig-col] position
        [[[block-row block-col]] block-text] (helpers/top-block-for contents position)
        prefix (get-prefix contents orig-row orig-col)
        ns-name (-> contents
                    (helpers/ns-range-for position)
                    second)
        [row col] (if range
                    [(- orig-row block-row) orig-col]
                    [0 0])]
    (if (= :compliment @cljs-autocomplete)
      (compliment/for-cljs clj-repl cmd ns-name block-text prefix row col)
      (simple/for-cljs cljs-repl ns-name prefix))))

(defn- resolve-clj [state opts editor-data resolve]
  (async/go
   (when (nil? @clj-autocomplete)
     (reset! clj-autocomplete
             (if (async/<! (detect-clj-compliment (:clj/aux @state)))
               :compliment
               :simple)))
   (resolve (async/<! (autocomplete-clj (:clj/aux @state) editor-data)))))

(defn- resolve-cljs [state opts editor-data resolve]
  (async/go
   (when (nil? @cljs-autocomplete)
     (reset! cljs-autocomplete
             (if (async/<! (detect-cljs-compliment (:clj/aux @state)))
               :compliment
               :simple)))
   (resolve (async/<! (autocomplete-cljs (:clj/aux @state)
                                         (:cljs/repl @state)
                                         (-> @state :repl/info :cljs/repl-env)
                                         editor-data)))))

(defn command [state {:keys [get-config] :as opts} editor-data]
  (js/Promise.
   (fn [resolve]
     (prn (:filename editor-data)
          (evaluation/need-cljs? (get-config) (:filename editor-data)))
     (if (evaluation/need-cljs? (get-config) (:filename editor-data))
       (resolve-cljs state opts editor-data resolve)
       (resolve-clj  state opts editor-data resolve)))))
