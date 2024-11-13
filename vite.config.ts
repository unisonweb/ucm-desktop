import { defineConfig } from "vite";
// @ts-ignore
import elmPlugin from 'vite-plugin-elm'
import path from "path";

const UI_CORE_SRC = "elm-stuff/gitdeps/github.com/unisonweb/ui-core/src";

// https://vitejs.dev/config/
export default defineConfig(async () => ({
  plugins: [elmPlugin({ debug: false })],

  resolve: {
    alias: {
      "ui-core": path.resolve(__dirname, UI_CORE_SRC)
    },
  },

  build: {
    target: "esnext"
  },

  // Vite options tailored for Tauri development and only applied in `tauri dev` or `tauri build`
  //
  // 1. prevent vite from obscuring rust errors
  clearScreen: false,
  // 2. tauri expects a fixed port, fail if that port is not available
  server: {
    port: 1420,
    strictPort: true,
    watch: {
      // 3. tell vite to ignore watching `src-tauri`
      ignored: ["**/src-tauri/**"],
    },
  },
}));
