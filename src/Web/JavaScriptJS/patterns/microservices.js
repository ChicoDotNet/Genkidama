const assert = require("node:assert/strict");
/** Models independently owned service boundaries communicating through explicit contracts. */
function run() {
  const inventoryService = { reserve: (sku) => ({ sku, reserved: true }) };
  const orderService = { place: (sku, inventory) => ({ orderId: "O1", inventory: inventory.reserve(sku) }) };
  const result = orderService.place("SKU-7", inventoryService);
  assert.deepEqual(result.inventory, { sku: "SKU-7", reserved: true });
  return "microservices";
}
module.exports = { run };
if (require.main === module) run();
