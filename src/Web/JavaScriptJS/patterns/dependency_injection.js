const assert = require("node:assert/strict");
/** Supplies dependencies from outside the consumer instead of constructing them internally. */
function run() {
  const service = (clock) => ({ timestamp: () => clock.now() });
  const fakeClock = { now: () => 123 };
  assert.equal(service(fakeClock).timestamp(), 123);
  return "dependency-injection";
}
module.exports = { run };
if (require.main === module) run();
