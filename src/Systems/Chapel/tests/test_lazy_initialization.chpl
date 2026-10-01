use PatternLazyInitialization;

proc main() {
  const actual = run();
  assert(actual == "created=1");
  writeln("chapel-cell-pass: lazy_initialization");
}
