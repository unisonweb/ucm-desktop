import { getCurrentWindow } from "@tauri-apps/api/window";
import { Menu, MenuItem, CheckMenuItem, Submenu, PredefinedMenuItem, } from "@tauri-apps/api/menu";
import * as Theme from "./Theme";
import * as AppSettings from "./AppSettings";

function changeThemeMenuItem(appSettings: AppSettings.AppSettings, submenu: Submenu, theme: Theme.Theme): Promise<CheckMenuItem> {
  const currentTheme = appSettings.theme;
  return CheckMenuItem.new({
    id: theme,
    text: Theme.prettyName(theme),
    checked: Theme.equals(currentTheme, theme),
    action: async (_) => {
      await updateThemeMenu(submenu, theme);
      await Theme.mount(theme);
      appSettings.theme = theme;
      await AppSettings.save(appSettings)
    }
  });
}

async function init(appSettings: AppSettings.AppSettings): Promise<Menu> {
  const menu = await Menu.new({
    id: "app",
    items: [],
  });

  const themeSubmenu = await Submenu.new({
    id: "theme-submenu",
    text: "Theme",
    items: []
  });

  themeSubmenu.append(await changeThemeMenuItem(appSettings, themeSubmenu, "system"));
  themeSubmenu.append(await changeThemeMenuItem(appSettings, themeSubmenu, "unison-light"));
  themeSubmenu.append(await changeThemeMenuItem(appSettings, themeSubmenu, "unison-dark"));

  const debugSubmenu = await Submenu.new({
    id: "debug-submenu",
    text: "Debug",
    items: [
      await MenuItem.new({
        id: "reload",
        text: "Reload",
        action: async (_) => window.location.reload()
      }),
      await MenuItem.new({
        id: "reset-to-factory-settings",
        text: "Reset to factory settings",
        action: async (_) => {
          await AppSettings.clear();
          window.location.reload();
        }
      })
    ]
  });

  const appSubmenu = await Submenu.new({
    id: "app-submenu",
    text: "App",
    items: [
      themeSubmenu,
      await PredefinedMenuItem.new({ item: "Separator" }),
      debugSubmenu,
      await PredefinedMenuItem.new({ item: "Services" }),
      await PredefinedMenuItem.new({ item: "Separator" }),
      await PredefinedMenuItem.new({ item: "Quit", text: "Quit" })
    ],
  });

  menu.append(appSubmenu);

  return menu;
}

async function mount(menu: Menu) {
  const win = getCurrentWindow();
  if (win.label === "main") menu.setAsAppMenu();
}

async function updateThemeMenu(themeSubmenu: Submenu, newTheme: Theme.Theme): Promise<Submenu> {
  const system = await (themeSubmenu.get("system") as Promise<CheckMenuItem>);
  system.setChecked(false);

  const unisonLight = await (themeSubmenu.get("unison-light") as Promise<CheckMenuItem>);
  unisonLight.setChecked(false);

  const unisonDark = await (themeSubmenu.get("unison-dark") as Promise<CheckMenuItem>);
  unisonDark.setChecked(false);

  const newThemeMenu = await (themeSubmenu.get(newTheme) as Promise<CheckMenuItem>);
  newThemeMenu.setChecked(true);

  return themeSubmenu;
}

export { updateThemeMenu, init, mount };
