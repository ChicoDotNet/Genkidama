const assert = require("node:assert/strict");
/** Exposes presentation-ready observable state through a view model. */
function run() {
  const model = { first: "Ada", last: "Lovelace" };
  const viewModel = {
    get fullName() { return `${model.first} ${model.last}`; },
    rename(first, last) { model.first = first; model.last = last; },
  };
  assert.equal(viewModel.fullName, "Ada Lovelace");
  viewModel.rename("Grace", "Hopper");
  assert.equal(viewModel.fullName, "Grace Hopper");
  return "mvvm";
}
module.exports = { run };
if (require.main === module) run();
