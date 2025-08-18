const rules = require("./webpack.rules");
const path = require("path");

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
