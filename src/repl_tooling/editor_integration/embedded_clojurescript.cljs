(ns repl-tooling.editor-integration.embedded-clojurescript
  (:require [repl-tooling.features.shadow-cljs :as shadow]
            [repl-tooling.integrations.connection :as conn]
            [repl-tooling.editor-integration.commands :as cmds]
            [repl-tooling.editor-helpers :as helpers]))

(def trs {:no-build-id "There's no build ID detected on shadow-cljs file"
          :no-shadow-file "File shadow-cljs.edn not found"
          :no-shadow "This project is not a shadow-cljs, can't connect to CLJS REPL"
          :workers-empty "No shadow-cljs workers running"
          :access-denied (str "Shadow Socket-REPL was given an wrong token. "
                              "Please, be sure you have the Shadow-CLJS compiler "
                              "running and watching some build-id")
          :timeout-runtime (str "Timeout trying to find runtime for Javascript, or "
                                "runtime for Javascript not found. If you're connecting "
                                "to a browser target, make sure that the browser is open, "
                                "on the page you want, and that JS compiled by "
                                "ClojureScript is loaded on the page. If it's not a "
                                "browser, make sure that the app that runs the "
                                "compiled Javascript is running")
          :no-worker "No worker for build"})

(defn- notify! [notify params]
  (notify params)
  (. js/Promise resolve nil))

(defn- treat-error [error notify]
  (let [message (cond-> error (keyword? error) (trs "Unknown Error"))]
    (notify! notify {:type :error
                     :title "Error connecting to ClojureScript"
                     :message message})
    nil))

(defn- save-repl-info! [state target repl]
  (swap! state
         (fn [s] (-> s
                     (assoc :cljs/repl repl)
                     (assoc-in [:repl/info :cljs/repl-env]
                               `(shadow.cljs.devtools.api/compiler-env ~target))))))

(defn- warn-html [title {:keys [line column msg file]}]
  (let [full-path (str file ":" line ":" column)
        norm-name (if (-> full-path count (> 60))
                    [:abbr {:title file :style {:border "none"
                                                :text-decoration "none"}}
                     (->> full-path (take-last 60) (apply str "..."))]
                    full-path)]

    [:<>
     [:div.col
      [:div.title.error title ": "]
      [:div.pre msg]]
     [:a {:href "#"
          :on-click (list 'fn '[_]
                          (list 'editor/run-callback
                                :open-editor {:file-name file
                                              :line (dec line)
                                              :column (dec column)}))}
      norm-name]
     [:div.space]]))

(defn- compile-error! [state compile-error]
  (let [txt (if (-> compile-error :type (= :warnings)) "Warning" "Error")
        id (gensym "shadow-error-")
        warnings (->> compile-error :warnings
                      (map #(warn-html txt %))
                      (cons :<>)
                      vec)
        interactive (pr-str (tagged-literal
                             'repl-tooling/interactive
                             {:html [:div.row
                                     [:div.title "Errors while compiling"]
                                     warnings]}))]
    (cmds/run-callback! state :on-start-eval {:id id
                                              :editor-data {:filename "<compile>.cljs"
                                                            :range [[0 0] [0 0]]
                                                            :contents ""}
                                              :range [[0 0] [0 0]]})
    (cmds/run-callback! state :on-eval {:id id
                                        :editor-data {:filename "<compile>.cljs"
                                                      :range [[0 0] [0 0]]
                                                      :contents ""}
                                        :range [[0 0] [0 0]]
                                        :repl nil
                                        :result (helpers/parse-result
                                                 {:result interactive
                                                  :as-text interactive})})))

(defn- connect-and-update-state! [state opts target upgrade-cmd]
  (let [{:keys [notify on-result on-stdout]} opts
        {:keys [host port]} (:repl/info @state)
        config (cmds/run-callback! state :get-config)
        after-connect #(if-let [error (:error %)]
                         (treat-error error notify)
                         (do
                           (save-repl-info! state target %)
                           (notify! notify
                                    {:type :info
                                     :title "Connected to ClojureScript"
                                     :message (str "Connected to Shadow-CLJS target " target)})
                           %))]
    (if target
      ; FIXME: feature toggle
      (if upgrade-cmd
        (.then (conn/connect-self-hosted! {:host host
                                           :port port
                                           :code upgrade-cmd
                                           :on-result #(and on-result (on-result %))
                                           :on-stdout #(and on-stdout (on-stdout %))})
               after-connect)
        (.then (conn/connect-shadow-ws! (assoc opts
                                               :directories (:project-paths config)
                                               :host host
                                               :port port
                                               :build-id target
                                               :compile-error #(compile-error! state %)))
               after-connect))
      (notify! notify {:type :warn
                       :title "No option selected"
                       :message "Please select a valid target for Shadow-CLJS"}))))

(defn- choose-id! [state {:keys [prompt] :as opts} commands use-new?]
  (.. (prompt {:title "Multiple Shadow-CLJS targets"
               :message "Choose the build target that you want to connect"
               :arguments (->> commands keys (map (fn [id] {:key id :value (name id)})))})
      (then #(connect-and-update-state! state opts
                                        (keyword %)
                                        (when-not use-new? (->> % keyword (get commands)))))))

(defn- connect-embedded [state {:keys [get-config notify] :as opts} use-new?]
  (let [commands (shadow/command-for (:project-paths (get-config)))]
    (if-let [error (:error commands)]
      (treat-error error notify)
      (case (count commands)
        0 (treat-error :no-build-id notify)
        1 (.then (connect-and-update-state! state opts
                                            (-> commands keys first)
                                            (when-not use-new? (-> commands vals first))))
        (choose-id! state opts commands use-new?)))))

(defn connect! [state {:keys [notify] :as opts} use-new?]
  (cond
    (:cljs/repl @state)
    (notify! notify {:type :warn
                     :title "REPL already connected"
                     :message (str "REPL is already connected.\n\n"
                                   "Please, disconnect the current REPL "
                                   "if you want to connect to another.")})

    (:clj/aux @state)
    (connect-embedded state opts use-new?)

    :else
    (notify! notify {:type :warn
                     :title "REPL not connected"
                     :message (str "To connect a self-hosted REPL, "
                                   "you first need to connect a Clojure REPL")})))
