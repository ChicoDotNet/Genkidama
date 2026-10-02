use PatternSingleton;

proc main() {
  const actual = run();
  assert(actual == "same=true;count=1");
  writeln("chapel-cell-pass: singleton");
}
