'use strict';

require("./styles.scss");

const devFlags = {
  apiBaseUrl: ELM_CONFIG.apiBaseUrl,
  apiProtocol: ELM_CONFIG.apiProtocol,
  apiPort: ELM_CONFIG.apiPort,
  dataPath: ELM_CONFIG.dataPath,
  imagePath: ELM_CONFIG.imagePath
}

const prodFlags = {
  apiBaseUrl: ELM_CONFIG.apiBaseUrl,
  apiProtocol: ELM_CONFIG.apiProtocol,
  apiPort: ELM_CONFIG.apiPort,
  dataPath: ELM_CONFIG.dataPath,
  imagePath: ELM_CONFIG.imagePath
}

const {Elm} = require('./Main');
const app = Elm.Main.init({
  flags: process.env.NODE_ENV === 'production' ? prodFlags : devFlags
});
