const assert = require("node:assert/strict");
/** Maps domain objects to storage rows without persistence behavior in the domain object. */
function run() {
  const rows = new Map();
  const mapper = {
    save(user) { rows.set(user.id, { user_id: user.id, display_name: user.name }); },
    find(id) { const row = rows.get(id); return { id: row.user_id, name: row.display_name }; },
  };
  const user = { id: 1, name: "Ada" };
  mapper.save(user);
  assert.deepEqual(mapper.find(1), user);
  return "data-mapper";
}
module.exports = { run };
if (require.main === module) run();
