# UCM Desktop
A desktop app companion to the UCM CLI. A.k.a. Graphical UCM.

![Alt](https://repobeats.axiom.co/api/embed/7b52b08fc59e1ae837f2fb4fbe95eac194262da5.svg "Repobeats analytics image")

---

This first version aims to replace [Unison
Local](https://github.com/unisonweb/unison-local-ui)

Later releases will include more overlap with the features of the UCM CLI and
indeed editing capabilities.


---

## Running 
[UCM the CLI](https://github.com/unisonweb/unison) needs to be running for the desktop app to connect to it. Simply start `ucm` however you usually do.

## Running for development
When running for development start UCM like so:

```bash
ucm --allow-cors-host http://localhost:1420
```

Then start the app with:

```bash
npm start
```
