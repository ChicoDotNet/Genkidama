use PatternModelViewPresenter;

proc main() {
  const actual = run();
  assert(actual == "view=ready");
  writeln("chapel-cell-pass: model_view_presenter");
}
