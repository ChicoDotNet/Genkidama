use PatternMemento;

proc main() {
  const actual = run();
  assert(actual == "restored=draft");
  writeln("chapel-cell-pass: memento");
}
