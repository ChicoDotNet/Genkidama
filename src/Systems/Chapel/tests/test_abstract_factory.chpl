use PatternAbstractFactory;

proc main() {
  const actual = run();
  assert(actual == "dark=button+checkbox");
  writeln("chapel-cell-pass: abstract_factory");
}
