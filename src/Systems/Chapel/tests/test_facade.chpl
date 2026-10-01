use PatternFacade;

proc main() {
  const actual = run();
  assert(actual == "checkout=charged");
  writeln("chapel-cell-pass: facade");
}
