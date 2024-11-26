# UCM Desktop
A desktop app companion to the UCM CLI. A.k.a. Graphical UCM.

---

This is currently an unreleased work in progress.

The goal is a UI that looks like this:

<img width="1872" alt="graphical-ucm" src="https://github.com/user-attachments/assets/34447b3d-4e1d-49c7-9171-634c09f5e1fb">

---

## Running 
UCM desktop requires the UCM CLI to be running on a specific port and with a
specific UCM token
```bash
UCM_TOKEN=asdf UCM_PORT=4444 ucm headless --allow-cors-host tauri://localhost
```

If on Windows, use this command instead:
```powershell
 $env:UCM_TOKEN="asdf"; $env:UCM_PORT="4444"; ucm headless --allow-cors-host https://tauri.localhost
```

Then start the UCM Desktop app as you would normally.

## Running for development
When running for development start UCM like so:

```bash
UCM_TOKEN=asdf UCM_PORT=4444 ucm headless --allow-cors-host http://localhost:1420
```

Then start the app with:

```bash
npm start
```
