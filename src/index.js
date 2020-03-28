'use strict';

require("./styles.scss");

const devFlags = {
  apiProtocol: "http",
  apiBaseUrl: ELM_CONFIG.dataApiUrl,
  apiPort: '3061',
  apiUrl: 'data'
}

const prodFlags = {
  apiProtocol: "https",
  apiBaseUrl: "d3n5xpivjiodis.cloudfront.net",
  apiPort: "none",
  apiUrl: "data.json"
}

const {Elm} = require('./Main');
const app = Elm.Main.init({
  flags: process.env.NODE_ENV === 'production' ? prodFlags : devFlags
});
