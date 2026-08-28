const assert = require("node:assert/strict");
/** Uses a behaviorally neutral collaborator instead of repeated null checks. */
function run() {
  const nullLogger = { log() {} };
  const memory = [];
  const realLogger = { log: (message) => memory.push(message) };
  const work = (logger = nullLogger) => logger.log("done");
  work();
  work(realLogger);
  assert.deepEqual(memory, ["done"]);
  return "null-object";
}
module.exports = { run };
if (require.main === module) run();
