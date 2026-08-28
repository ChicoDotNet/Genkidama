const assert = require("node:assert/strict");
/** Keeps a small core stable while plugins extend named capabilities. */
function run() {
  const plugins = new Map();
  const kernel = {
    register(name, plugin) { plugins.set(name, plugin); },
    execute(name, input) { return plugins.get(name)(input); },
  };
  kernel.register("uppercase", (value) => value.toUpperCase());
  assert.equal(kernel.execute("uppercase", "genkidama"), "GENKIDAMA");
  return "microkernel";
}
module.exports = { run };
if (require.main === module) run();
