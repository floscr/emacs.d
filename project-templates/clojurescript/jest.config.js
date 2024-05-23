const path = require("path");

module.exports = {
  // The preset you installed earlier, which allows Jest to work with `cljest` and ClojureScript files
  preset: "jest-preset-cljest",

  testEnvironment: "jsdom",

  // A setup file for loading CLJS specific things like `is`
  setupFilesAfterEnv: [path.resolve(__dirname, "jest.setup.js")],
};
