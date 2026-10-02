use PatternComposite;

proc main() {
  const actual = run();
  assert(actual == "root=10");
  writeln("chapel-cell-pass: composite");
}
