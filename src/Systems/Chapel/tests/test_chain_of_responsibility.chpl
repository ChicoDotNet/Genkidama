use PatternChainOfResponsibility;

proc main() {
  const actual = run();
  assert(actual == "handled=billing");
  writeln("chapel-cell-pass: chain_of_responsibility");
}
