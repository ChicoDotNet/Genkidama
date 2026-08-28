const assert = require("node:assert/strict");
/** Coordinates colleagues through a mediator instead of direct coupling. */
function run() {
  const events = [];
  const mediator = {
    notify(sender, event) {
      if (event === "order-placed") events.push(`reserve:${sender.orderId}`, `bill:${sender.orderId}`);
    },
  };
  const checkout = { orderId: "A-7", place() { mediator.notify(this, "order-placed"); } };
  checkout.place();
  assert.deepEqual(events, ["reserve:A-7", "bill:A-7"]);
  return "mediator";
}
module.exports = { run };
if (require.main === module) run();
