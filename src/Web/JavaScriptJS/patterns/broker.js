const assert = require("node:assert/strict");
/** Routes requests through a broker so clients do not know service locations. */
function run() {
  const services = new Map([["tax", (amount) => amount * 0.16]]);
  const broker = { call: (service, payload) => services.get(service)(payload) };
  assert.equal(broker.call("tax", 100), 16);
  return "broker";
}
module.exports = { run };
if (require.main === module) run();
