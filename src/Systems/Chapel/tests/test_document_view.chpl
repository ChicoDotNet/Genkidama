use PatternDocumentView;

proc main() {
  const actual = run();
  assert(actual == "views=2");
  writeln("chapel-cell-pass: document_view");
}
