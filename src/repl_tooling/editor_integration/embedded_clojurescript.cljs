(ns repl-tooling.editor-integration.embedded-clojurescript
  (:require [repl-tooling.features.shadow-cljs :as shadow]
            [cljs.core.async :include-macros true :as async]
            [repl-tooling.integrations.connection :as conn]
            [repl-tooling.eval :as eval]))

(def trs {:no-build-id "There's no build ID detected on shadow-cljs file"
          :no-shadow-file "File shadow-cljs.edn not found"
          :no-worker "No worker for first build ID"})

(defn- treat-error [error notify]
  (notify {:type :error
           :title "Error connecting to ClojureScript"
           :message (trs error "Unknown Error")})
  nil)

(defn- connect-and-update-state! [state notify host port build-id]
  (.. (conn/connect! host port build-id prn)
      (then #(if-let [error (:error %)]
               (treat-error error notify)
               (do (swap! state assoc :cljs/repl %) state)))))

(defn- choose-id! [state notify commands prompt host port resolve]
  (.. (prompt {:title "Multiple Shadow-CLJS targets"
               :message "Choose the build target that you want to connect"
               :values (->> commands keys (map name))})
      (then #(connect-and-update-state! state notify
                                        host port (->> % keyword (get commands))))
      (then resolve)))

(defn- connect-embedded [state {:keys [notify prompt get-config]} resolve]
  (let [project-paths (:project-paths (get-config))
        commands (shadow/command-for project-paths)
        {:keys [host port]} (:repl/info @state)]
    (if-let [error (:error commands)]
      (resolve (treat-error error notify))
      (case (count commands)
        0 (resolve (treat-error :no-build-id notify))
        1 (.then (connect-and-update-state! state notify
                                            host port (-> commands vals first))
                 resolve)
        (choose-id! state notify commands prompt host port resolve)))))

(defn connect! [state {:keys [notify prompt] :as opts}]
  (js/Promise.
   (fn [resolve]
     (cond
       (:cljs/repl @state)
       (do
         (resolve nil)
         (notify {:type :warn
                  :title "REPL already connected"
                  :message (str "REPL is already connected.\n\n"
                                "Please, disconnect the current REPL "
                                "if you want to connect to another.")}))

       (:clj/aux @state)
       (connect-embedded state opts resolve)

       :else
       (do
         (resolve nil)
         (notify {:type :warn
                  :title "REPL not connected"
                  :message (str "To connect a self-hosted REPL, "
                                "you first need to connect a Clojure REPL")}))))))
