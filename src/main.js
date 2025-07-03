import { app, BrowserWindow } from 'electron';
import started from 'electron-squirrel-startup';

const COLORS = {
  grayLighten100: "rgba(255, 255, 255, 0)",
  grayLighten60: "#fafafb",
  grayLighten30: "#bdbfc6",
  grayLighten20: "#818286",
  grayDarken10: "#2d2e35",
  grayDarken20: "#22232a",
}

const THEME = {
  light: {
    chrome: COLORS.grayLighten60,
    chromeEmphasized: COLORS.grayLighten100,
    icon: COLORS.grayLighten20
  },
  dark: {
    chrome: COLORS.grayDarken20,
    chromeEmphasized: COLORS.grayDarken10,
    icon: COLORS.grayLighten30
  },
};

// Handle creating/removing shortcuts on Windows when installing/uninstalling.
if (started) {
  app.quit();
}

const createWindow = () => {
  // for Linux and Windows
  const titleBarOverlay = {
    titleBarOverlay: {
      color: THEME.light.chromeEmphasized,
      symbolColor: THEME.light.icon,
      height: 39
    }
  };

  // Create the browser window.
  const mainWindow = new BrowserWindow({
    width: 1024,
    height: 768,
    titleBarStyle: 'hidden',
    ...(process.platform !== 'darwin' ? titleBarOverlay : {}),
    trafficLightPosition: { x: 12, y: 12 },
    webPreferences: {
      preload: MAIN_WINDOW_PRELOAD_WEBPACK_ENTRY,
    },
  });

  // and load the index.html of the app.
  mainWindow.loadURL(MAIN_WINDOW_WEBPACK_ENTRY);
};

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.whenReady().then(() => {
  createWindow();

  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      createWindow();
    }
  });
});

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and import them here.
