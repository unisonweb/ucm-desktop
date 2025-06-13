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
import detectOs from "ui-core/Lib/detectOs";
import preventDefaultGlobalKeyboardEvents from "ui-core/Lib/preventDefaultGlobalKeyboardEvents";
import "./main.css";
import * as AppError from "./Ucm/AppError";
import * as AppSettings from "./Ucm/AppSettings";
import * as Theme from "./Ucm/Theme";
import appIcon from "./assets/app-icon.png";

const assets = {
  appIcon,
};

// @ts-ignore
import { Elm } from './Main.elm';

// In development, requests are proxied via the Webpack devServer to UCM.
const UCM_API_URL = "http://127.0.0.1:5858/codebase/api";
const API_URL = process.env.NODE_ENV === "development" ? "/codebase/api" : UCM_API_URL;

try {
  console.log("-- Starting UCM Desktop -------------------------------------");

  preventDefaultGlobalKeyboardEvents();

  // -- AppSettings -----------------------------------------------------------
  const appSettings = AppSettings.init();
  const operatingSystem = detectOs(window.navigator);

  // -- Elm -------------------------------------------------------------------
  const flags = {
    operatingSystem: operatingSystem,
    basePath: "",
    apiUrl: API_URL,
    workspaceContext: appSettings.workspaceContexts[0],
    theme: appSettings.theme,
    assets,
  };

  const app = Elm.Main.init({ flags });

  if (app.ports) {
    app.ports.saveWorkspaceContext?.subscribe(async (workspaceContext) => {
      appSettings.workspaceContexts = [workspaceContext];
      AppSettings.save(appSettings);
    });

    app.ports.saveTheme?.subscribe(async (theme) => {
      appSettings.theme = theme
      AppSettings.save(appSettings);
      Theme.mount(theme);
    });

    app.ports.reloadApp?.subscribe(() => window.location.reload());
    app.ports.clearSettings?.subscribe(() => {
      AppSettings.clear();
      window.location.reload();
    });
  }

  // -- CSS env classes -------------------------------------------------------
  // Set things like an OS specific class and the current theme
  // /!\ This sas to happen _after_ Elm.Main.init, otherwise <body> is overwritten.
  if (flags.operatingSystem) {
    document.querySelector("body")?.classList.add(flags.operatingSystem.toLowerCase());
  }
  Theme.mount(appSettings.theme);
  Theme.listenToSystemChange();
}
catch (ex) {
  console.error(ex);
  AppError.mount(AppError.init(ex));
}
