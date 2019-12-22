'use strict';

require("./styles.scss");

const {Elm} = require('./Main');
const app = Elm.Main.init({
  flags: {
    apiBaseUrl: "localhost",
    apiPort: "3061",
    apiUrl: "data"
  }
});