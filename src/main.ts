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
import * as AppError from "./Ucm/AppError";
import * as AppSettings from "./Ucm/AppSettings";
import * as Theme from "./Ucm/Theme";
import * as WindowMenu from "./Ucm/WindowMenu";

// @ts-ignore
import { Elm } from './Main.elm';

try {
  console.log("-- Starting UCM Desktop -------------------------------------");

  preventDefaultGlobalKeyboardEvents();

  // -- AppSettings -----------------------------------------------------------
  const appSettings = await AppSettings.init();

  // -- WindowMenu ------------------------------------------------------------
  const menu = await WindowMenu.init(appSettings);
  WindowMenu.mount(menu);

  // -- Elm -------------------------------------------------------------------
  const flags = {
    operatingSystem: detectOs(window.navigator),
    basePath: "",
    apiUrl: "http://127.0.0.1:5858/codebase/api",
    workspaceContext: appSettings.workspaceContexts[0],
    theme: appSettings.theme
  };

  const app = Elm.Main.init({ flags });

  if (app.ports) {
    app.ports.saveWorkspaceContext?.subscribe(async (workspaceContext: AppSettings.WorkspaceContext) => {
      appSettings.workspaceContexts = [workspaceContext];
      AppSettings.save(appSettings);
    });

    app.ports.saveTheme?.subscribe(async (theme: Theme.Theme) => {
      appSettings.theme = theme
      AppSettings.save(appSettings);
    });

    app.ports.clearSettings?.subscribe(AppSettings.clear);
  }

  // -- CSS env classes -------------------------------------------------------
  // Set things like an os specific class and the current theme
  // /!\ Has to happen _after_ Elm.Main.init, otherwise <body> is overwritten.
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
