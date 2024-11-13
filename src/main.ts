import { Store } from '@tauri-apps/plugin-store'
import { getCurrentWindow } from "@tauri-apps/api/window";

import "ui-core/css/ui.css";
import "ui-core/css/themes/unison-light.css";
import "ui-core/css/code.css";
// Include web components
import "ui-core/UI/CopyOnClick";
import "ui-core/UI/ModalOverlay";
import "ui-core/UI/CopyrightYear";
import "ui-core/Lib/OnClickOutside";
import "ui-core/Lib/EmbedKatex";
import "ui-core/Lib/MermaidDiagram";
import "ui-core/Lib/EmbedSvg";
// @ts-ignore
import detectOs from "ui-core/Lib/detectOs";
// @ts-ignore
import preventDefaultGlobalKeyboardEvents from "ui-core/Lib/preventDefaultGlobalKeyboardEvents";
import "./main.css";

console.log("Starting UCM");

// @ts-ignore
import { Elm } from './Main.elm';

type WorkspaceContext = {
  projectName: string;
  branchRef: string
}

console.log("Loading Store");
const store = await Store.load('settings.json');
console.log("Store loaded");

const workspaceContexts = (await store.get<Array<WorkspaceContext>>('workspace-contexts') || []);

const unlisten = await getCurrentWindow().onCloseRequested(async (ev) => {
  console.log("TODO");
  ev.preventDefault();
});

// you need to call unlisten if your handler goes out of scope e.g. the component is unmounted
unlisten();

// ELM STUFF

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath: "",
  apiUrl: "http://127.0.0.1:4444/asdf/api",
  workspaceContext: workspaceContexts[0],
};

preventDefaultGlobalKeyboardEvents();

console.log("Starting Elm app");
const app = Elm.Main.init({ flags });
console.log("Elm app started");

if (app.ports) {
  app.ports.saveWorkspaceContext?.subscribe(async (workspaceContext: WorkspaceContext) => {
    console.log("saving contexts", workspaceContext);
    try {
      await store.set("workspace-contexts", [workspaceContext])
    } catch (ex) {
      console.error(ex);
    }
  });
}
