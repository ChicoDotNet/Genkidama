use PatternActiveRecord;

proc main() {
  const actual = run();
  assert(actual == "saved=42");
  writeln("chapel-cell-pass: active_record");
}
