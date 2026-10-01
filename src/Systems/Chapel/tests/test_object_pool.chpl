use PatternObjectPool;

proc main() {
  const actual = run();
  assert(actual == "reused=true");
  writeln("chapel-cell-pass: object_pool");
}
