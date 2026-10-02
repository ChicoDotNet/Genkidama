use PatternServiceLocator;

proc main() {
  const actual = run();
  assert(actual == "service=mail");
  writeln("chapel-cell-pass: service_locator");
}
