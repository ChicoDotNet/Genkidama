use PatternActiveObject;

proc main() {
  const actual = run();
  assert(actual == "result=9");
  writeln("chapel-cell-pass: active_object");
}
