use PatternMvc;

proc main() {
  const actual = run();
  assert(actual == "view=5");
  writeln("chapel-cell-pass: mvc");
}
