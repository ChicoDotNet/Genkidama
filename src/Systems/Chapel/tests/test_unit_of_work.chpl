use PatternUnitOfWork;

proc main() {
  const actual = run();
  assert(actual == "commits=2");
  writeln("chapel-cell-pass: unit_of_work");
}
