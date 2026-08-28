const assert = require("node:assert/strict");
/** Keeps a passive view behind an interface driven by a presenter. */
function run() {
  const model = { load: () => ({ name: "Ada" }) };
  const view = { text: "", showName(value) { this.text = value; } };
  const presenter = { start() { view.showName(model.load().name); } };
  presenter.start();
  assert.equal(view.text, "Ada");
  return "model-view-presenter";
}
module.exports = { run };
if (require.main === module) run();
