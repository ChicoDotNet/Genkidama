const assert = require("node:assert/strict");
/** Couples a record with persistence operations for its own row. */
function run() {
  const table = new Map();
  class User {
    constructor(id, name) { this.id = id; this.name = name; }
    save() { table.set(this.id, { id: this.id, name: this.name }); }
    static find(id) { const row = table.get(id); return row && new User(row.id, row.name); }
  }
  new User(1, "Ada").save();
  assert.equal(User.find(1).name, "Ada");
  return "active-record";
}
module.exports = { run };
if (require.main === module) run();
