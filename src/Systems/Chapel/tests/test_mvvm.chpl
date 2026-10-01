use PatternMvvm;

proc main() {
  const actual = run();
  assert(actual == "label=5");
  writeln("chapel-cell-pass: mvvm");
}
