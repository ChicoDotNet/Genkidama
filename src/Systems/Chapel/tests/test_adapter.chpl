use PatternAdapter;

proc main() {
  const actual = run();
  assert(actual == "celsius=100");
  writeln("chapel-cell-pass: adapter");
}
