const assert = require("node:assert/strict");
/** Changes behavior by replacing the current state object. */
function run() {
  const locked = { open: () => "locked", unlock: (door) => { door.state = unlocked; } };
  const unlocked = { open: () => "opened", unlock: () => {} };
  const door = { state: locked, open() { return this.state.open(this); }, unlock() { this.state.unlock(this); } };
  assert.equal(door.open(), "locked");
  door.unlock();
  assert.equal(door.open(), "opened");
  return "state";
}
module.exports = { run };
if (require.main === module) run();
