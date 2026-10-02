use PatternPrototype;

proc main() {
  const actual = run();
  assert(actual == "clone=tracing");
  writeln("chapel-cell-pass: prototype");
}
