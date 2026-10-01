use PatternMessageBus;

proc main() {
  const actual = run();
  assert(actual == "delivered=2");
  writeln("chapel-cell-pass: message_bus");
}
