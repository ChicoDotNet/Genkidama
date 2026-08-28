const assert = require("node:assert/strict");
/** Selects a pricing algorithm independently from the checkout context. */
function run() {
  const price = (amount, strategy) => strategy(amount);
  const regular = (amount) => amount;
  const vip = (amount) => amount * 0.8;
  assert.equal(price(100, regular), 100);
  assert.equal(price(100, vip), 80);
  return "strategy";
}
module.exports = { run };
if (require.main === module) run();
