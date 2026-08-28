const assert = require("node:assert/strict");
/** Allows multiple views to render the same document model independently. */
function run() {
  const document = { title: "Report", rows: [1, 2] };
  const tableView = { render: (doc) => `${doc.title}:${doc.rows.join(",")}` };
  const summaryView = { render: (doc) => `${doc.title}(${doc.rows.length})` };
  assert.equal(tableView.render(document), "Report:1,2");
  assert.equal(summaryView.render(document), "Report(2)");
  return "document-view";
}
module.exports = { run };
if (require.main === module) run();
