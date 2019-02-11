const path = require('path')
const {app, BrowserWindow} = require('electron')

app.on('ready', () => {
  let browser = new BrowserWindow({
    width: 900, height: 600, nodeIntegration: true})
  browser.loadURL(path.join('file://', __dirname, '/index.html'))
})
