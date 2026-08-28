const assert = require("node:assert/strict");
/** Decouples business abstraction from interchangeable enterprise transports. */
function run() {
  const http = { send: (payload) => `http:${payload}` };
  const queue = { send: (payload) => `queue:${payload}` };
  const notifier = (transport) => ({ notify: (message) => transport.send(message) });
  assert.equal(notifier(http).notify("x"), "http:x");
  assert.equal(notifier(queue).notify("x"), "queue:x");
  return "enterprise-bridge";
}
module.exports = { run };
if (require.main === module) run();
