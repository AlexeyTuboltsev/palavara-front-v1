'use strict';

require("./styles.scss");

const {Elm} = require('./Main');
const app = Elm.Main.init({
  flags: {
    apiProtocol: "http",
    apiBaseUrl: 'localhost',
    apiPort: '3061',
    apiUrl: 'data'
  }
});

// apiProtocol: "https",
//   apiBaseUrl: "d3n5xpivjiodis.cloudfront.net",
//   apiPort: "none",
//   apiUrl: "data.json"


// apiBaseUrl: "localhost",
//   apiPort: "3061",
//   apiUrl: "data"