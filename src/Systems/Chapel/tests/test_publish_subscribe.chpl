use PatternPublishSubscribe;

proc main() {
  const actual = run();
  assert(actual == "subscribers=2");
  writeln("chapel-cell-pass: publish_subscribe");
}
