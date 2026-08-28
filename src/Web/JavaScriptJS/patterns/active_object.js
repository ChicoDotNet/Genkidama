const assert = require("node:assert/strict");
/** Decouples method submission from execution through an owned request queue. */
function run() {
  const queue = [];
  const results = [];
  const active = {
    submit(fn) { queue.push(fn); },
    drain() { while (queue.length) results.push(queue.shift()()); },
  };
  active.submit(() => 2 + 3);
  assert.deepEqual(results, []);
  active.drain();
  assert.deepEqual(results, [5]);
  return "active-object";
}
module.exports = { run };
if (require.main === module) run();
