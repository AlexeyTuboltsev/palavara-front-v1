const networkInterfaces = require('os').networkInterfaces();

const getLocalIp = () => {
  return Object.values(networkInterfaces).reduce((ip,nIntfce) => {
    const findIp = findExternalIp(nIntfce);
    console.log("findIp",findIp)
    return ip === null && findIp !== null ? findIp : ip
  }, null)
};

const findExternalIp = (networkInterface) => networkInterface.reduce((ip, nItfce) => {
  console.log("nItfce",nItfce)
  return (ip === null && nItfce.internal === false && nItfce.family === 'IPv4') ? nItfce.address : ip
}, null);

module.exports = getLocalIp();
