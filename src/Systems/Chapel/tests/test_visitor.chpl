use PatternVisitor;

proc main() {
  const actual = run();
  assert(actual == "area=18");
  writeln("chapel-cell-pass: visitor");
}
