use PatternHalfSyncHalfAsync;

proc main() {
  const actual = run();
  assert(actual == "handled=3");
  writeln("chapel-cell-pass: half_sync_half_async");
}
