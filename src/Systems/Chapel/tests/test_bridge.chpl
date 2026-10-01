use PatternBridge;

proc main() {
  const actual = run();
  assert(actual == "tv=muted");
  writeln("chapel-cell-pass: bridge");
}
