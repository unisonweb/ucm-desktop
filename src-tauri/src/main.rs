// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use tauri::Manager;
use tauri_plugin_decorum::WebviewWindowExt;

fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_shell::init())
        .plugin(tauri_plugin_store::Builder::default().build())
        .plugin(tauri_plugin_decorum::init())
        .setup(|app| {
          #[cfg(target_os = "macos")] {
            let main_window = app.get_webview_window("main").unwrap();
            main_window.create_overlay_titlebar().unwrap();

            // 11 instead of 12 to account for left border
            main_window.set_traffic_lights_inset(11.0, 16.0).unwrap();
          }

          Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
