(ns repl-tooling.integration.ui
  (:require [reagent.core :as r]
            [clojure.test :as t :include-macros true]
            [check.core :refer-macros [check]]
            [check.async :include-macros true]
            [clojure.core.async :as async :include-macros true]
            [devcards.core :as cards :include-macros true]

            [repl-tooling.editor-integration.connection :as conn]))

(defonce state (r/atom {:host "localhost"
                        :port 2233
                        :code "(+ 1 2)"
                        :repls {:eval nil
                                :aux nil}
                        :commands {}
                        :stdout nil
                        :stderr nil
                        :eval-result nil}))

(defn disconnect! []
  (conn/disconnect!))

(defn handle-disconnect []
  (swap! state assoc
         :repls {:eval nil :aux nil}
         :stdout nil :stderr nil :eval-result nil
         :commands {}))

(defn connect! []
  (.
   (conn/connect! (:host @state) (:port @state)
                  {:on-disconnect handle-disconnect
                   :on-stdout #(swap! state update :stdout (fn [e] (str e %)))
                   :on-eval #(swap! state update :stdout (fn [e] (str e "=> " (:as-text %) "\n")))
                   :on-stderr #(swap! state update :stderr (fn [e] (str e %)))
                   :editor-data #(let [code (:code @state)]
                                    {:contents code})})
   (then (fn [res]
           (swap! state assoc :repls {:eval (:clj/repl res)
                                      :aux (:clj/aux res)}
                  :commands (:editor/commands res)
                  :stdout "" :stderr "")))))

(defn fake-editor [state]
  [:div
   [:h4 "Socket REPL connections"]
   [:p [:b "Hostname: "] [:input {:type "text" :value (:host @state)
                                  :on-change #(->> % .-target .-value (swap! state assoc :host))}]
       [:b " Port: "] [:input {:type "text" :value (:port @state)
                               :on-change #(->> % .-target .-value int (swap! state assoc :port))}]]
   [:textarea {:style {:width "100%" :height "200px"}
               :on-change #(->> % .-target .-value (swap! state assoc :code))}
    (:code @state)]
   [:p
    (if (-> @state :repls :eval)
      [:span
       [:button {:on-click #((-> @state :commands :evaluate-selection :command))}
        "Evaluate"] " "
       [:button {:on-click disconnect!} "Disconnect!"]]
      [:button {:on-click connect!} "Connect!"])]
   [:p (if (-> @state :repls :eval) "Connected" "Disconnected")]
   (when-let [out (:stdout @state)]
     [:div
      [:h5 "STDOUT"]
      [:pre out]])
   (when-let [out (:stderr @state)]
     [:div
      [:h5 "STDERR"]
      [:pre out]])])

(cards/defcard-rg fake-editor
  fake-editor
  state
  {:inspect-data true})

(cards/deftest connect-repl
  (t/async done
    (let [promise (conn/connect-unrepl! "localhost" 2233
                                        {:on-stdout identity
                                         :on-stderr identity
                                         :on-result identity
                                         :on-disconnect identity})
          c (async/promise-chan)
          r (async/promise-chan)]
      (async/go
       (.then promise #(async/put! c (:commands %)))
       ; (.log js/console (:evaluate (async/<! c)))
       ; (prn :CMD (async/<! c))
       ((:evaluate (async/<! c)) "(+ 1 2)" {} #(async/put! c (:result %)))
       (check r =resolves=> 3)
       ; (conn/disconnect!)
       (done)))))
    ; (check {:foo 1} => {:foo 2})))

(defn main [])
;   (r/render [app] (. js/document (querySelector "#app"))))

; (main)
(cards/start-devcard-ui!)
; (devcards.core/start-devcard-ui!)


  ; (println "HELLO" (.-app electron))
  ;
  ; (.. electron
  ;     -app
  ;     (on "ready"
  ;         #(let [win (new (.-BrowserWindow electron) #js {:width 900 :height 600})]
  ;            (.. win (loadURL "about:blank"))
  ;            (prn (.. win -webContents (insertText "FOOBAR")))
  ;            (.. win -webContents (executeJavaScript "alert(document.querySelector('body'))"))))))
  ; mainWindow = new BrowserWindow({width: 900, height: 600}));

; var BrowserWindow = require('browser-window');  // Module to create native browser window.
;
; // Keep a global reference of the window object, if you don't, the window will
; // be closed automatically when the JavaScript object is garbage collected.
; var mainWindow = null;
;
; // Quit when all windows are closed.
; app.on('window-all-closed', function() {})
;     // On OS X it is common for applications and their menu bar
;     // to stay active until the user quits explicitly with Cmd + Q
;     if (process.platform != 'darwin') {}
;         app.quit();
;
; ;
;
; // This method will be called when Electron has finished
; // initialization and is ready to create browser windows.
; app.on('ready', function() {})
;     // Create the browser window.
;     mainWindow = new BrowserWindow({width: 900, height: 600});
;
;     // and load the index.html of the app.
;     mainWindow.loadURL('file://' + __dirname + '/index.html');
;
;     // Emitted when the window is closed.
;     mainWindow.on('closed', function() {})
;         // Dereference the window object, usually you would store windows
;         // in an array if your app supports multi windows, this is the time
;         // when you should delete the corresponding element.
;         mainWindow = null;
;     ;
; ;
