use PatternMicrokernel;

proc main() {
  const actual = run();
  assert(actual == "plugin=16");
  writeln("chapel-cell-pass: microkernel");
}
