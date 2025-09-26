# UCM Desktop

A desktop app companion to the UCM CLI. Supports code and documentation browsing via the same core display tech used on [Unison Share](https://share.unison-lang.org). All code is hyperlinked, with type signatures and syntax hints shown on hover. Also supports keyboard navigation and both light and dark modes.

<img width="2880" height="1750" alt="CleanShot 2025-09-25 at 20 45 38@2x" src="https://github.com/user-attachments/assets/2b864d4c-2e4b-4482-a998-63e16744d61d" />

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
