use PatternPresentationAbstractionControl;

proc main() {
  const actual = run();
  assert(actual == "agents=2");
  writeln("chapel-cell-pass: presentation_abstraction_control");
}
