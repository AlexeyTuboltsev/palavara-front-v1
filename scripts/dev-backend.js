'use strict';

const http = require('http');
const https = require('https');
const fs = require('fs');
const path = require('path');

const PORT = 3061;
const PROD_HOST = 'data.palavara.com';
const DATA_FILE = path.join(__dirname, '../mock-data/data.json');

const server = http.createServer((req, res) => {
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');

  if (req.method === 'OPTIONS') {
    res.writeHead(200);
    res.end();
    return;
  }

  if (req.url === '/data') {
    fs.readFile(DATA_FILE, (err, data) => {
      if (err) {
        res.writeHead(500);
        res.end('Data file not found: ' + DATA_FILE);
        return;
      }
      res.setHeader('Content-Type', 'application/json');
      res.writeHead(200);
      res.end(data);
    });
  } else {
    // Proxy everything else (images, etc.) to production
    const options = {
      hostname: PROD_HOST,
      path: req.url,
      method: req.method,
    };

    const proxyReq = https.request(options, (proxyRes) => {
      res.writeHead(proxyRes.statusCode, proxyRes.headers);
      proxyRes.pipe(res);
    });

    proxyReq.on('error', (err) => {
      res.writeHead(502);
      res.end('Proxy error: ' + err.message);
    });

    proxyReq.end();
  }
});

server.listen(PORT, () => {
  console.log(`Dev backend running at http://localhost:${PORT}`);
  console.log(`Serving /data from: ${DATA_FILE}`);
  console.log(`Proxying /img/* to: https://${PROD_HOST}`);
});
