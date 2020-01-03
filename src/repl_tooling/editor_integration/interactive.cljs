(ns repl-tooling.editor-integration.interactive
  (:require [reagent.core :as r]
            [reagent.ratom :as a]
            [schema.core :as s]
            [clojure.walk :as walk]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]))

; (s/def ::this any?)
; (s/def ::atom #(instance? a/RAtom %))
; (s/def ::repl #(instance? eval/Evaluator %))
; (s/def ::editor-state any?)
; (s/def ::root? boolean?)
;
; (s/def ::dispatch
;   (s/fspec :args (s/cat :this vector?)))
;
; (s/def ::renderer
;   (s/fspec :args (s/cat :opts (s/keys :req-un [::this ::dispatch]))
;            :ret vector?))
;
; (s/def ::event
;   (s/fspec :args (s/cat :opts (s/keys :req-un [::state ::repl ::editor-state ::dispatch]))
;            :ret any?))
;
(def renderer {:this s/Any :dispatch s/Any})
(def Event {:state s/Any
            :repl s/Any
            :editor-state s/Any
            :dispatch s/Any})
(defonce ^:private renderers (atom {}))
(defonce ^:private events (atom {}))

(defn- render [{:keys [this dispatch]}]
  (let [edn (second this)
        obj (dispatch [:assign-renderable edn])]
     (helpers/as-html @obj obj true)))

(defn- normalize-html-actions [reagent-params {:keys [dispatch]}]
  (->> reagent-params
       (map (fn [[key val]]
              (if (and (keyword? key)
                       (->> key name (re-find #"^on-"))
                       (vector? val))
                [key (fn [e]
                       (.preventDefault e)
                       (.stopPropagation e)
                       (dispatch val))]
                [key val])))
       (into {})))

(defn- normalize-tags [vector params]
  (let [event (get @renderers (first vector))
        p-params (second vector)]
    (cond
      event (event (assoc params :this vector))
      (map? p-params) (update vector 1 normalize-html-actions params)
      :else vector)))

(defn- html [{:keys [this] :as params}]
  (let [hiccup (second this)]
    (walk/prewalk #(cond-> % (vector? %) (normalize-tags params)) hiccup)))

(defn register-renderer!
  [key renderer]
  (swap! renderers assoc key renderer))

(defn reset-renderers! []
  (reset! renderers {})
  (register-renderer! :render render)
  (register-renderer! :html html))
(reset-renderers!)

(defn- dispatcher-fn [state repl editor-state]
  (let [params {:state state :repl repl :editor-state editor-state}]
    (fn dispatcher [event]
      (if-let [evt (get @events (first event))]
        (evt event (assoc params :dispatch  dispatcher))
        ;FIXME add error
        (prn :no-dispatcher-for (first event))))))

(defrecord Interactive [edn repl editor-state]
  helpers/Renderable
  (as-html [_ ratom _]
    (let [renderer (-> edn first)
          renderer (get @renderers renderer)
          dispatcher (dispatcher-fn ratom repl editor-state)]
      [renderer {:this edn
                 :dispatch dispatcher
                 :editor-state editor-state}])))

  ; as-text)

;; EVENTS
(defn register-event!
  [key event]
  (swap! events assoc key event))

(defn assign-renderable [[_ edn] {:keys [state repl editor-state]}]
  (if-let [renderable (get-in @state [:additional-states edn])]
    renderable
    (let [renderable (helpers/as-renderable edn repl editor-state)]
      (swap! state assoc-in [:additional-states edn] renderable)
      renderable)))

(defn- replace-view [[_ this] {:keys [state]}]
  (swap! state assoc :edn this))

(defn- evaluate [[_ code] {:keys [editor-state dispatch]}]
  (when-let [eval (-> @editor-state :editor/features :eval)]
    (let [{:keys [range editor-data]} (meta editor-state)
          [_ namespace] (helpers/ns-range-for (:contents editor-data) (first range))
          eval (eval code {:filename (:filename editor-data)
                           :range range
                           :namespace (str namespace)
                           :ignore true
                           :pass {:interactive true}})]
      (.then eval #(do
                     (.log js/console :DISP dispatch)
                     (prn :RES %)
                     (dispatch %)))
      (.catch eval prn))))

(defn reset-events! []
  (reset! events {})
  (register-event! :replace replace-view)
  (register-event! :assign-renderable assign-renderable)
  (register-event! :eval evaluate))
(reset-events!)
