(ns repl-tooling.editor-integration.interactive
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer-macros [defn-spec]]
            [reagent.core :as r]
            [reagent.ratom :as a]
            [clojure.walk :as walk]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]))

(s/def ::this any?)
(s/def ::atom #(instance? a/RAtom %))
(s/def ::repl #(instance? eval/Evaluator %))
(s/def ::editor-state any?)
(s/def ::root? boolean?)
(s/def ::renderer (s/fspec :args (s/cat :opts (s/keys :req-un [::this ::repl ::editor-state ::root?]))
                           :ret vector?))
(s/def ::event (s/fspec :args (s/cat :opts (s/keys :req-un [::atom ::repl ::editor-state ::root?]))
                        :ret any?))

(defonce ^:private renderers (atom {}))
(defonce ^:private events (atom {}))

(defn- render [{:keys [this repl editor-state root?]}]
  (let [edn (second this)
        obj (helpers/as-renderable edn repl editor-state)]
    (helpers/as-html @obj obj root?)))

(defn- normalize-html-actions [params]
  (->> params
       (map (fn [[key val]]
              (if (and (keyword? key) (->> key name (re-find #"^on-")))
                [key (fn [e]
                       (.preventDefault e)
                       (.stopPropagation e)
                       (prn val))]
                [key val])))
       (into {})))

(defn- normalize-tags [vector params]
  (let [event (get @renderers (first vector))
        p-params (second vector)]
    (cond
      event (event (assoc params :this vector))
      (map? p-params) (update vector 1 normalize-html-actions)
      :else vector)))

(defn- html [{:keys [this] :as params}]
  (let [hiccup (second this)]
    (walk/prewalk #(cond-> % (vector? %) (normalize-tags params)) hiccup)))

(defn-spec register-renderer! any?
  [key keyword?, renderer ::renderer]
  (swap! renderers assoc key renderer))

(defn reset-renderers! []
  (reset! renderers {})
  (register-renderer! :render render)
  (register-renderer! :html html))
(reset-renderers!)

(defrecord Interactive [edn repl editor-state]
  helpers/Renderable
  (as-html [_ ratom root?]
    (let [renderer (-> @ratom :edn first)
          renderer (get @renderers renderer)]
      [renderer {:this edn
                 :repl repl
                 :editor-state editor-state
                 :root? root?}])))
    ; [:div (pr-str edn)]))

  ; as-text)

;; EVENTS
(defn-spec register-event! any?
  [key keyword?, event ::event]
  (swap! events assoc key event))

(defn reset-events! []
  (reset! events {}))
  ; (register-event! :render render)
  ; (register-event! :html html))
(reset-events!)
