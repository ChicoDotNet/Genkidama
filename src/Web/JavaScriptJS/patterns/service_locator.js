const assert = require("node:assert/strict");
/** Resolves registered services from a locator while making the trade-off explicit. */
function run() {
  const services = new Map();
  const locator = { register: (name, service) => services.set(name, service), get: (name) => services.get(name) };
  locator.register("clock", { now: () => 123 });
  assert.equal(locator.get("clock").now(), 123);
  return "service-locator";
}
module.exports = { run };
if (require.main === module) run();
