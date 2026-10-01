use PatternDistributedProxy;

proc main() {
  const actual = run();
  assert(actual == "remote=42");
  writeln("chapel-cell-pass: distributed_proxy");
}
