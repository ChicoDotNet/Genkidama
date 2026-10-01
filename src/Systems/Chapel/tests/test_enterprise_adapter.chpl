use PatternEnterpriseAdapter;

proc main() {
  const actual = run();
  assert(actual == "erp=42");
  writeln("chapel-cell-pass: enterprise_adapter");
}
