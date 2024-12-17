# UCM Desktop
A desktop app companion to the UCM CLI. A.k.a. Graphical UCM.

---

This is currently an unreleased work in progress.

The goal is a UI that looks like this:

<img width="1872" alt="graphical-ucm" src="https://github.com/user-attachments/assets/34447b3d-4e1d-49c7-9171-634c09f5e1fb">

---

## Running 
UCM desktop requires the UCM CLI to be running, simply start it with the `ucm`
command (Mac/Linux):
```bash
UCM_TOKEN=codebase UCM_PORT=5858 ucm --allow-cors-host tauri://localhost
```
Windows (Powershell):
```
$env:UCM_TOKEN="codebase"; $env:UCM_PORT="5858"; ucm --allow-cors-host http://tauri.localhost
```

Then start the UCM Desktop app as you would normally.

## Running for development
When running for development start UCM like so:

```bash
UCM_TOKEN=codebase UCM_PORT=5858 ucm --allow-cors-host http://localhost:1420
```

Then start the app with:

```bash
npm start
```
