import { Store } from '@tauri-apps/plugin-store'
import * as Theme from "./Theme";

export type WorkspaceContext = {
  projectName: string;
  branchRef: string
}

export type AppSettings = {
  workspaceContexts: Array<WorkspaceContext>
  theme: Theme.Theme
};

let store: Store | null = null;

async function init(): Promise<AppSettings> {
  console.log("Loading AppSettings");
  store = await Store.load("settings.json");

  const workspaceContexts = (await store.get<Array<WorkspaceContext>>("workspace-contexts") || []);
  const theme = (await store.get<Theme.Theme>("theme") || "system");

  const appSettings = {
    workspaceContexts: workspaceContexts,
    theme: theme
  }

  console.log("AppSettings loaded", appSettings);

  return appSettings
}

async function save(appSettings: AppSettings): Promise<AppSettings> {
  if (store) {
    try {
      console.log("Saving AppSettings", appSettings);
      await store.set("workspace-contexts", appSettings.workspaceContexts)
      await store.set("theme", appSettings.theme)
    } catch (ex) {
      console.error(ex);
    }
  }

  return appSettings;
}

async function clear() {
  if (store) {
    try {
      await store.delete("workspace-contexts");
      await store.delete("theme");
    } catch (ex) {
      console.error(ex);
    }
  }
}

export { init, save, clear };
