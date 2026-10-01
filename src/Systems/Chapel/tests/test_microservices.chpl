use PatternMicroservices;

proc main() {
  const actual = run();
  assert(actual == "order=paid");
  writeln("chapel-cell-pass: microservices");
}
