use PatternEnterpriseBridge;

proc main() {
  const actual = run();
  assert(actual == "sap>json");
  writeln("chapel-cell-pass: enterprise_bridge");
}
