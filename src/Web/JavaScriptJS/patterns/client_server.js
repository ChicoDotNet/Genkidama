const assert = require("node:assert/strict");
/** Separates a request-making client from a request-serving boundary. */
function run() {
  const server = { handle: ({ path }) => path === "/health" ? { status: 200, body: "ok" } : { status: 404 } };
  const client = { get: (path) => server.handle({ method: "GET", path }) };
  assert.deepEqual(client.get("/health"), { status: 200, body: "ok" });
  return "client-server";
}
module.exports = { run };
if (require.main === module) run();
