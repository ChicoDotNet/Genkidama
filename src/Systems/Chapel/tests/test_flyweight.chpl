use PatternFlyweight;

proc main() {
  const actual = run();
  assert(actual == "styles=2;shared=true");
  writeln("chapel-cell-pass: flyweight");
}
