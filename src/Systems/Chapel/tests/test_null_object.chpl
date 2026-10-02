use PatternNullObject;

proc main() {
  const actual = run();
  assert(actual == "messages=0");
  writeln("chapel-cell-pass: null_object");
}
