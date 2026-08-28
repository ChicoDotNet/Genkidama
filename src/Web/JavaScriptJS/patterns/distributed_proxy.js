const assert = require("node:assert/strict");
/** Hides remote invocation details behind a local-looking proxy contract. */
function run() {
  const transport = { request: (method, id) => ({ method, id, name: "Ada" }) };
  const userProxy = { get: (id) => transport.request("GET /users/:id", id) };
  assert.deepEqual(userProxy.get("42"), { method: "GET /users/:id", id: "42", name: "Ada" });
  return "distributed-proxy";
}
module.exports = { run };
if (require.main === module) run();
