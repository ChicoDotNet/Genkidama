use PatternDataMapper;

proc main() {
  const actual = run();
  assert(actual == "entity=42");
  writeln("chapel-cell-pass: data_mapper");
}
