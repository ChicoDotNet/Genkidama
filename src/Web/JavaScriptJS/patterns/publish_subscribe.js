const assert = require("node:assert/strict");
/** Decouples publishers and subscribers by topic. */
function run() {
  const topics = new Map();
  const broker = {
    subscribe(topic, fn) { const xs = topics.get(topic) ?? []; xs.push(fn); topics.set(topic, xs); },
    publish(topic, value) { for (const fn of topics.get(topic) ?? []) fn(value); },
  };
  const seen = [];
  broker.subscribe("orders", (value) => seen.push(value.id));
  broker.publish("orders", { id: 7 });
  assert.deepEqual(seen, [7]);
  return "publish-subscribe";
}
module.exports = { run };
if (require.main === module) run();
