// See the Electron documentation for details on how to use preload scripts:
// https://www.electronjs.org/docs/latest/tutorial/process-model#preload-scripts
//
import { contextBridge, ipcRenderer } from 'electron';

contextBridge.exposeInMainWorld('electronAPI', {
  ptyInput: (command) => ipcRenderer.send('pty-input', command),
  onPtyOutput: (callback) => {
    ipcRenderer.on('pty-output', (_event, value) => {
      callback(value);
    });
  },
})
