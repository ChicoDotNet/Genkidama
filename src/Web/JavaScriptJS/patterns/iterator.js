const assert = require("node:assert/strict");
/** Traverses a collection without exposing its storage representation. */
function run() {
  const playlist = {
    tracks: new Set(["intro", "theme", "credits"]),
    *[Symbol.iterator]() { yield* this.tracks; },
  };
  assert.deepEqual([...playlist], ["intro", "theme", "credits"]);
  return "iterator";
}
module.exports = { run };
if (require.main === module) run();
