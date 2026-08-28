const assert = require("node:assert/strict");
/** Lets peers both request and provide data without a distinguished central server. */
function run() {
  const peers = new Map();
  const peer = (name) => ({ name, share(key, value) { peers.set(`${name}:${key}`, value); }, fetch(owner, key) { return peers.get(`${owner}:${key}`); } });
  const a = peer("a"), b = peer("b");
  a.share("doc", "hello");
  assert.equal(b.fetch("a", "doc"), "hello");
  return "peer-to-peer";
}
module.exports = { run };
if (require.main === module) run();
