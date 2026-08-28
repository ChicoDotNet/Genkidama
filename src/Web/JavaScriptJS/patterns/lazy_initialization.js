const assert = require("node:assert/strict");
/** Defers expensive creation until the value is first requested and then reuses it. */
function run() {
  let builds = 0;
  let cached;
  const resource = () => cached ??= { id: ++builds };
  assert.equal(builds, 0);
  assert.equal(resource(), resource());
  assert.equal(builds, 1);
  return "lazy-initialization";
}
module.exports = { run };
if (require.main === module) run();
