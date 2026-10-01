use PatternCommand;

proc main() {
  const actual = run();
  assert(actual == "balance=15;undo=10");
  writeln("chapel-cell-pass: command");
}
