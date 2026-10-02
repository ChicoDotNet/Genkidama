use PatternState;

proc main() {
  const actual = run();
  assert(actual == "locked>unlocked");
  writeln("chapel-cell-pass: state");
}
