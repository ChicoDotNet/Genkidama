use PatternDecorator;

proc main() {
  const actual = run();
  assert(actual == "audit(enc(alert))");
  writeln("chapel-cell-pass: decorator");
}
