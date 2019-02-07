(ns repl-tooling.integration.ui
  ; (:require ["electron" :as electron]
  ;           ["fs" :as fs]))
  ; (:require ["fs" :as fs]))
  (:require [reagent.core :as r]
            [sablono.core :as sab]
            [devcards.core :as cards :include-macros true]))
  ; (:require-macros [devcards.core :refer [defcard]]))

; (defn js-for-browser []
;   (js/alert "FOO"))

(defn app []
  (r/as-element [:h1 "Lalala"]))

(cards/defcard reagent-macro-1
  (cards/reagent [:div "This works fine"]))

(cards/defcard-rg example
  [app])

(cards/defcard-rg isolating-state
  (fn [data-atom _]
    [:div
     [:p (:count @data-atom)]
     [:p [:button {:on-click #(swap! data-atom update :count inc)} "Inc"]]])
  (r/atom {:count 0}) ; <-- intial ratom
  {:inspect-data true :history true})

(defn main []
  (r/render [app] (. js/document (querySelector "#app"))))

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