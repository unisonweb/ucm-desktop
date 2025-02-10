const rules = require('./webpack.rules');
const path = require('path');
const CopyPlugin = require("copy-webpack-plugin");

const UI_CORE_SRC = "elm-stuff/gitdeps/github.com/unisonweb/ui-core/src";

rules.push({
  test: /\.css$/,
  use: [{ loader: 'style-loader' }, { loader: 'css-loader' }],
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

  plugins: [
    new CopyPlugin({
      patterns: [
        {
          from: "src/assets/app-icon.png",
          to: "app-icon.png",
        },
      ]
    }),
  ]
};
