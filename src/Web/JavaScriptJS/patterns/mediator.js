const assert = require("node:assert/strict");

/** Coordinates colleagues through a mediator instead of direct coupling. */
function run() {
  const events = [];
  const colleagues = new Map([
    ["inventory", (sender, message) => events.push(`inventory<-${sender}:${message}`)],
    ["payment", (sender, message) => events.push(`payment<-${sender}:${message}`)],
  ]);

  const mediator = {
    send(sender, recipient, message) {
      const receiver = colleagues.get(recipient);
      if (!receiver) throw new Error(`Unknown colleague: ${recipient}`);
      receiver(sender, message);
    },
  };

  const payment = {
    send(message) {
      mediator.send("payment", "inventory", message);
    },
  };

  const inventory = {
    send(message) {
      mediator.send("inventory", "payment", message);
    },
  };

  payment.send("paid");
  inventory.send("reserved");

  assert.deepEqual(events, [
    "inventory<-payment:paid",
    "payment<-inventory:reserved",
  ]);
  assert.throws(
    () => mediator.send("payment", "shipping", "paid"),
    /Unknown colleague: shipping/,
  );

  return "mediator";
}

module.exports = { run };
if (require.main === module) run();
