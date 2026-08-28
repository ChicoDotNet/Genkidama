const assert = require("node:assert/strict");
/** Executes the Command example and verifies undoable encapsulated work. */
function run() {
  const account = { balance: 100 };
  const deposit = {
    amount: 25,
    execute() { account.balance += this.amount; },
    undo() { account.balance -= this.amount; },
  };
  deposit.execute();
  assert.equal(account.balance, 125);
  deposit.undo();
  assert.equal(account.balance, 100);
  return "command";
}
module.exports = { run };
if (require.main === module) run();
