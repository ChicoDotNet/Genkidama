const assert = require("node:assert/strict");
/** Evaluates a tiny expression language through composable expression nodes. */
function run() {
  const number = (value) => ({ interpret: () => value });
  const add = (left, right) => ({ interpret: () => left.interpret() + right.interpret() });
  const multiply = (left, right) => ({ interpret: () => left.interpret() * right.interpret() });
  const expression = add(number(2), multiply(number(3), number(4)));
  assert.equal(expression.interpret(), 14);
  return "interpreter";
}
module.exports = { run };
if (require.main === module) run();
