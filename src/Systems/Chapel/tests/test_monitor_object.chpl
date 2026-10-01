use PatternMonitorObject;

proc main() {
  const actual = run();
  assert(actual == "count=5");
  writeln("chapel-cell-pass: monitor_object");
}
