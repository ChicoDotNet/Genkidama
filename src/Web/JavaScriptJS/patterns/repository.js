const assert = require("node:assert/strict");
/** Presents collection-like domain access while hiding persistence details. */
function run() {
  const rows = new Map();
  const users = {
    add(user) { rows.set(user.id, { ...user }); },
    get(id) { const row = rows.get(id); return row && { ...row }; },
  };
  users.add({ id: 1, name: "Ada" });
  assert.deepEqual(users.get(1), { id: 1, name: "Ada" });
  return "repository";
}
module.exports = { run };
if (require.main === module) run();
