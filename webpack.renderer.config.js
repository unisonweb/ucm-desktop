const path = require("path");

// The renderer runs without node integration and has no native modules, so
// drop the native module rules. @vercel/webpack-asset-relocator-loader would
// otherwise inject a `__dirname` reference into the runtime, which throws
// "__dirname is not defined" in the renderer and preload.
const NATIVE_MODULE_LOADERS = [
  "node-loader",
  "@vercel/webpack-asset-relocator-loader",
];
const rules = require("./webpack.rules").filter((rule) => {
  const loader = typeof rule.use === "string" ? rule.use : rule.use?.loader;
  return !NATIVE_MODULE_LOADERS.includes(loader);
});

const UI_CORE_SRC = "elm-stuff/gitdeps/github.com/unisonweb/ui-core/src";

rules.push({
  test: /\.css$/,
  use: [{ loader: "style-loader" }, { loader: "css-loader" }],
});

rules.push({
  test: /\.(png|svg|jpg|jpeg|gif)$/i,
  type: "asset/resource",
});

module.exports = {
  // Put your normal webpack config below here
  module: {
    rules,
  },

  resolve: {
    alias: {
      assets: path.resolve(__dirname, "src/assets/"),
      "ui-core": path.resolve(__dirname, UI_CORE_SRC + "/"),
    },
  },
};
