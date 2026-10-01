use PatternStrategy;

proc main() {
  const actual = run();
  assert(actual == "square=16");
  writeln("chapel-cell-pass: strategy");
}
