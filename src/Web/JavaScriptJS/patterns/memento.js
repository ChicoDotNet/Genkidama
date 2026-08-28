const assert = require("node:assert/strict");
/** Captures and restores editor state without exposing restoration internals. */
function run() {
  const editor = {
    text: "",
    snapshot() { return Object.freeze({ text: this.text }); },
    restore(memento) { this.text = memento.text; },
  };
  editor.text = "draft";
  const saved = editor.snapshot();
  editor.text = "broken";
  editor.restore(saved);
  assert.equal(editor.text, "draft");
  return "memento";
}
module.exports = { run };
if (require.main === module) run();
