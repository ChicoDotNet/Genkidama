use PatternEnterpriseFacade;

proc main() {
  const actual = run();
  assert(actual == "customer=ok");
  writeln("chapel-cell-pass: enterprise_facade");
}
