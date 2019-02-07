const path = require('path')
const {app, BrowserWindow} = require('electron')

app.on('ready', () => {
  let browser = new BrowserWindow({width: 900, height: 600})
  browser.loadURL(path.join('file://', __dirname, '/index.html'))
})
  //
  // (.. electron
  //     -app
  //     (on "ready"
  //         #(let [win (new (.-BrowserWindow electron) #js {:width 900 :height 600})]
  //            (.. win (loadURL "about:blank"))
  //            (prn (.. win -webContents (insertText "FOOBAR")))
  //            (.. win -webContents (executeJavaScript "alert(document.querySelector('body'))"))))))
