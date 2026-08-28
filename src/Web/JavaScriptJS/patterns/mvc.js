const assert = require("node:assert/strict");
/** Separates model state, view rendering and controller input handling. */
function run() {
  const model = { count: 0 };
  const view = { render: (m) => `count=${m.count}` };
  const controller = { increment() { model.count += 1; return view.render(model); } };
  assert.equal(controller.increment(), "count=1");
  return "mvc";
}
module.exports = { run };
if (require.main === module) run();
