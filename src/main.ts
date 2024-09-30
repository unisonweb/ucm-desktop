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

import { Elm } from './Main.elm'

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath: new URL(document.baseURI).pathname,
  apiUrl: "http://127.0.0.1:4444/asdf/api"
};

preventDefaultGlobalKeyboardEvents();

Elm.Main.init({ flags });
