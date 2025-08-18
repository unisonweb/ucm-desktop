let store = window.localStorage;

function init() {
  console.log("Loading AppSettings");

  const workspaceContexts = getWorkspaceContexts();
  const theme = store.getItem("theme") || "system";

  const appSettings = {
    workspaceContexts: workspaceContexts,
    theme: theme,
  };

  console.log("AppSettings loaded", appSettings);

  return appSettings;
}

function getWorkspaceContexts() {
  const raw = store.getItem("workspace-contexts");
  if (raw) {
    return JSON.parse(raw);
  } else {
    return [];
  }
}

function save(appSettings) {
  if (store) {
    try {
      console.log("Saving AppSettings", appSettings);
      store.setItem(
        "workspace-contexts",
        JSON.stringify(appSettings.workspaceContexts),
      );
      store.setItem("theme", appSettings.theme);
    } catch (ex) {
      console.error(ex);
    }
  }

  return appSettings;
}

function clear() {
  if (store) {
    try {
      store.clear();
    } catch (ex) {
      console.error(ex);
    }
  }
}

export { init, save, clear };
