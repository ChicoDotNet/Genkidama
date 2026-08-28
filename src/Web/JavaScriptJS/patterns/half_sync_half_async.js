const assert = require("node:assert/strict");
/** Buffers asynchronous ingress before a synchronous processing layer. */
async function run() {
  const queue = [];
  const asyncIngress = async (value) => { queue.push(await Promise.resolve(value)); };
  const syncWorker = () => queue.splice(0).map((value) => value.toUpperCase());
  await asyncIngress("event");
  assert.deepEqual(syncWorker(), ["EVENT"]);
  return "half-sync-half-async";
}
module.exports = { run };
if (require.main === module) run().catch((error) => { console.error(error); process.exitCode = 1; });
