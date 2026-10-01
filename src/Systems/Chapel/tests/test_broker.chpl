use PatternBroker;

proc main() {
  const actual = run();
  assert(actual == "route=worker-b");
  writeln("chapel-cell-pass: broker");
}
