const assert = require("node:assert/strict");
/** Rotates leadership so one worker accepts work while followers wait to take over. */
function run() {
  const workers = ["w1", "w2", "w3"];
  let leader = 0;
  const accept = (job) => {
    const handledBy = workers[leader];
    leader = (leader + 1) % workers.length;
    return `${handledBy}:${job}`;
  };
  assert.deepEqual([accept("a"), accept("b")], ["w1:a", "w2:b"]);
  return "leader-followers";
}
module.exports = { run };
if (require.main === module) run();
