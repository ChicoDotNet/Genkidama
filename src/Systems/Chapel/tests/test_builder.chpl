use PatternBuilder;

proc main() {
  const actual = run();
  assert(actual == "service=99.95");
  writeln("chapel-cell-pass: builder");
}
