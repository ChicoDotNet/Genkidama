use PatternRepository;

proc main() {
  const actual = run();
  assert(actual == "found=42");
  writeln("chapel-cell-pass: repository");
}
