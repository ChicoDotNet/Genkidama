use PatternInterpreter;

proc main() {
  const actual = run();
  assert(actual == "result=7");
  writeln("chapel-cell-pass: interpreter");
}
