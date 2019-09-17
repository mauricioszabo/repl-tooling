(ns repl-tooling.editor-integration.embedded-clojurescript
  (:require [repl-tooling.features.shadow-cljs :as shadow]
            [cljs.core.async :include-macros true :as async]
            [repl-tooling.integrations.connection :as conn]))

(def trs {:no-build-id "There's no build ID detected on shadow-cljs file"
          :no-shadow-file "File shadow-cljs.edn not found"
          :no-worker "No worker for first build ID"})

(defn- treat-error [error notify]
  (notify {:type :error
           :title "Error connecting to ClojureScript"
           :message (trs error "Unknown Error")})
  nil)

(defn- connect-and-update-state! [state opts target upgrade-cmd]
  (let [{:keys [notify on-result on-stdout]} opts
        {:keys [host port]} (:repl/info @state)]
    (.. (conn/connect! host port upgrade-cmd {:on-result #(and on-result (on-result %))
                                              :on-stdout #(and on-stdout (on-stdout %))})
        (then #(if-let [error (:error %)]
                 (treat-error error notify)
                 (do
                   (swap! state assoc :cljs/repl %)
                   (notify {:type :info
                            :title "Connected to ClojureScript"
                            :message (str "Connected to Shadow-CLJS target" target)})))))))

(defn- choose-id! [state {:keys [prompt] :as opts} commands]
  (.. (prompt {:title "Multiple Shadow-CLJS targets"
               :message "Choose the build target that you want to connect"
               :values (->> commands keys (map (fn [id] {:key id :value (name id)})))})
      (then #(connect-and-update-state! state opts
                                        (keyword %)
                                        (->> % keyword (get commands))))))

(defn- connect-embedded [state {:keys [get-config notify] :as opts}]
  (let [commands (shadow/command-for (:project-paths (get-config)))]
    (if-let [error (:error commands)]
      (treat-error error notify)
      (case (count commands)
        0 (treat-error :no-build-id notify)
        1 (.then (connect-and-update-state! state opts
                                            (-> commands keys first)
                                            (-> commands vals first)))
        (choose-id! state opts commands)))))

(defn connect! [state {:keys [notify] :as opts}]
  (cond
    (:cljs/repl @state)
    (do
      (notify {:type :warn
               :title "REPL already connected"
               :message (str "REPL is already connected.\n\n"
                             "Please, disconnect the current REPL "
                             "if you want to connect to another.")}))

    (:clj/aux @state)
    (connect-embedded state opts)

    :else
    (do
      (notify {:type :warn
               :title "REPL not connected"
               :message (str "To connect a self-hosted REPL, "
                             "you first need to connect a Clojure REPL")}))))
