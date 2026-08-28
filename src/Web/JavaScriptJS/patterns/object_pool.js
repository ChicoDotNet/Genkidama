const assert = require("node:assert/strict");
/** Reuses bounded expensive objects instead of creating one per request. */
function run() {
  const available = [{ id: 1 }, { id: 2 }];
  const pool = { acquire: () => available.pop(), release: (item) => available.push(item) };
  const item = pool.acquire();
  assert.equal(available.length, 1);
  pool.release(item);
  assert.equal(available.length, 2);
  return "object-pool";
}
module.exports = { run };
if (require.main === module) run();
