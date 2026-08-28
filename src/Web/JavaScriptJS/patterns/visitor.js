const assert = require("node:assert/strict");
/** Adds an operation across heterogeneous nodes without changing node classes. */
function run() {
  class Text { constructor(value) { this.value = value; } accept(visitor) { return visitor.text(this); } }
  class Image { constructor(bytes) { this.bytes = bytes; } accept(visitor) { return visitor.image(this); } }
  const sizeVisitor = { text: (node) => node.value.length, image: (node) => node.bytes };
  assert.equal([new Text("abc"), new Image(5)].reduce((n, x) => n + x.accept(sizeVisitor), 0), 8);
  return "visitor";
}
module.exports = { run };
if (require.main === module) run();
