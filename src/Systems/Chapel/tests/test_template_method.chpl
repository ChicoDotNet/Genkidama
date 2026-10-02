use PatternTemplateMethod;

proc main() {
  const actual = run();
  assert(actual == "steps=validate>persist");
  writeln("chapel-cell-pass: template_method");
}
