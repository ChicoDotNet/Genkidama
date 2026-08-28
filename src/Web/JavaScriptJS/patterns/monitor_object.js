const assert = require("node:assert/strict");
/** Encapsulates shared state and serializes access through a monitor-owned queue. */
function run() {
  const monitor = {
    value: 0,
    criticalSection(action) { return action(this); },
    increment() { this.criticalSection((self) => { self.value += 1; }); },
  };
  monitor.increment(); monitor.increment();
  assert.equal(monitor.value, 2);
  return "monitor-object";
}
module.exports = { run };
if (require.main === module) run();
