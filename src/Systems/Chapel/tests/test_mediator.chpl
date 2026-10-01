use PatternMediator;

proc main() {
  const actual = run();
  assert(actual == "alice>bob=hello");
  writeln("chapel-cell-pass: mediator");
}
