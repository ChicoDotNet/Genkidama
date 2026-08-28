const assert = require("node:assert/strict");
/** Keeps workflow order stable while subclasses customize selected steps. */
function run() {
  class ImportJob {
    execute() { return [this.read(), this.normalize(), this.persist()].join(">"); }
    read() { return "read"; }
    normalize() { return "normalize"; }
    persist() { return "persist"; }
  }
  class CsvImport extends ImportJob { normalize() { return "normalize-csv"; } }
  assert.equal(new CsvImport().execute(), "read>normalize-csv>persist");
  return "template-method";
}
module.exports = { run };
if (require.main === module) run();
