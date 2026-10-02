use PatternFactoryMethod;

proc main() {
  const actual = run();
  assert(actual == "postgresql=connected");
  writeln("chapel-cell-pass: factory_method");
}
