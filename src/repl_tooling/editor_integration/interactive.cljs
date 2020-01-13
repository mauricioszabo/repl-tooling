(ns repl-tooling.editor-integration.interactive
  (:require [clojure.walk :as walk]
            [repl-tooling.eval :as eval]
            [reagent.core :as r]
            [repl-tooling.editor-helpers :as helpers]))

(defonce ^:private renderers (atom {}))
(defonce ^:private events (atom {}))

(defn- render [{:keys [this dispatch]}]
  (let [edn (second this)
        obj (dispatch [:assign-renderable edn])]
     (r/as-element (helpers/as-html @obj obj true))))

(defn- normalize-html-actions [reagent-params {:keys [dispatch]}]
  (->> reagent-params
       (map (fn [[key val]]
              (prn :NORM key val)
              (if (and (keyword? key)
                       (->> key name (re-find #"^on-"))
                       (vector? val))
                [key (fn [e]
                       (prn :CLICKED)
                       (.preventDefault e)
                       (.stopPropagation e)
                       (prn :REALLY-CLICKED)
                       (dispatch val)
                       (prn :LOL))]
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

(defn- interactive [{:keys [this dispatch]}]
  (let [interactive (-> this second helpers/Interactive.)
        obj (dispatch [:assign-renderable interactive])]
    (r/as-element (helpers/as-html @obj obj true))))

(defn register-renderer!
  [key renderer]
  (swap! renderers assoc key renderer))

(defn reset-renderers! []
  (reset! renderers {})
  (register-renderer! :render render)
  (register-renderer! :html html)
  (register-renderer! :interactive interactive))
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

;; EVENTS
(defn register-event!
  [key event]
  (swap! events assoc key event))

(defn assign-renderable [[_ edn] {:keys [state repl editor-state]}]
  (let [edn-s (pr-str edn)]
    (if-let [renderable (get-in @state [:additional-states edn-s])]
      renderable
      (let [renderable (helpers/as-renderable edn repl editor-state)]
        (swap! state assoc-in [:additional-states edn-s] renderable)
        renderable))))

(defn- replace-view [[_ this] {:keys [state]}]
  (swap! state assoc :edn this))

(defn- evaluate [[_ code maybe-opts] {:keys [editor-state dispatch] :as args}]
  (when-let [evaluate (-> @editor-state :editor/features :eval)]
    (let [eval-opts (or maybe-opts {})
          evaluate (evaluate code (-> eval-opts
                                      (assoc :ignore true)
                                      (assoc-in [:pass :interactive] true)))]
      (.then evaluate #(dispatch (:result %)))
      (when-let [rescue (:on-error eval-opts)]
        (.catch evaluate #(rescue % args))))))

(defn reset-events! []
  (reset! events {})
  (register-event! :replace replace-view)
  (register-event! :assign-renderable assign-renderable)
  (register-event! :eval evaluate))
(reset-events!)
