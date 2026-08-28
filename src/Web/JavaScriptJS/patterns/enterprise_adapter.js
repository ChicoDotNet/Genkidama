const assert = require("node:assert/strict");
/** Adapts an external enterprise contract to the application's canonical shape. */
function run() {
  const legacyErp = { findCustomer: () => ({ customer_no: "42", display_name: "Ada" }) };
  const customerGateway = { get: () => { const x = legacyErp.findCustomer(); return { id: x.customer_no, name: x.display_name }; } };
  assert.deepEqual(customerGateway.get(), { id: "42", name: "Ada" });
  return "enterprise-adapter";
}
module.exports = { run };
if (require.main === module) run();
