const assert = require("node:assert/strict");
/** Tracks changes and commits them as one explicit unit. */
function run() {
  const store = [];
  const pending = [];
  const uow = { add(entity) { pending.push(entity); }, commit() { store.push(...pending.splice(0)); } };
  uow.add({ id: 1 });
  assert.equal(store.length, 0);
  uow.commit();
  assert.equal(store.length, 1);
  return "unit-of-work";
}
module.exports = { run };
if (require.main === module) run();
