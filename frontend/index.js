'use strict';
const electron = require('electron');
const app = electron.app;
const ipcMain = electron.ipcMain;
const api = require('./src/api');

// adds debug features like hotkeys for triggering dev tools and reload
require('electron-debug')();

// prevent window being garbage collected
let mainWindow;

function onClosed() {
	  // dereference the window
	  // for multiple windows store them in an array
	  mainWindow = null;
}

function createMainWindow() {
	  const win = new electron.BrowserWindow({
		    width: 600,
		    height: 400
	  });

	  win.loadURL(`file://${__dirname}/index.html`);
	  win.on('closed', onClosed);

	  return win;
}

app.on('window-all-closed', () => {
	  if (process.platform !== 'darwin') {
		    app.quit();
	  }
});

app.on('activate', () => {
	  if (!mainWindow) {
		    mainWindow = createMainWindow();
	  }
});

app.on('ready', () => {
	  mainWindow = createMainWindow();
});

// processing ipc
function startChartProcessing () {
    ipcMain.send
}

ipcMain.on('fileform-channel', (e, config) => {
    api.initialize(config)
        .then(function (data) {
            console.log(data);
            e.sender.send('chart-initialize');
        })
        .catch(function (errData) {
            console.log(errData.error);
        });
});
