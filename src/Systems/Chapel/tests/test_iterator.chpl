use PatternIterator;

proc main() {
  const actual = run();
  assert(actual == "sum=6");
  writeln("chapel-cell-pass: iterator");
}
