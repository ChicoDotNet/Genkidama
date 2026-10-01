use PatternDependencyInjection;

proc main() {
  const actual = run();
  assert(actual == "result=8");
  writeln("chapel-cell-pass: dependency_injection");
}
