use PatternClientServer;

proc main() {
  const actual = run();
  assert(actual == "response=pong");
  writeln("chapel-cell-pass: client_server");
}
