use PatternObserver;

proc main() {
  const actual = run();
  assert(actual == "events=2");
  writeln("chapel-cell-pass: observer");
}
