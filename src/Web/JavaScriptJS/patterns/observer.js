const assert = require("node:assert/strict");
/** Publishes state changes to independently subscribed observers. */
function run() {
  const listeners = new Set();
  const seen = [];
  const subject = {
    subscribe(listener) { listeners.add(listener); return () => listeners.delete(listener); },
    publish(value) { for (const listener of listeners) listener(value); },
  };
  const unsubscribe = subject.subscribe((value) => seen.push(value));
  subject.publish("ready");
  unsubscribe();
  subject.publish("ignored");
  assert.deepEqual(seen, ["ready"]);
  return "observer";
}
module.exports = { run };
if (require.main === module) run();
