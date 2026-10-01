use PatternProxy;

proc main() {
  const actual = run();
  assert(actual == "fetches=1");
  writeln("chapel-cell-pass: proxy");
}
