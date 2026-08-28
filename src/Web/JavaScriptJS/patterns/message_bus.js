const assert = require("node:assert/strict");
/** Routes typed messages to handlers through a shared bus. */
function run() {
  const routes = new Map();
  const bus = {
    on(type, handler) { routes.set(type, handler); },
    send(message) { return routes.get(message.type)(message); },
  };
  bus.on("sum", ({ a, b }) => a + b);
  assert.equal(bus.send({ type: "sum", a: 2, b: 3 }), 5);
  return "message-bus";
}
module.exports = { run };
if (require.main === module) run();
