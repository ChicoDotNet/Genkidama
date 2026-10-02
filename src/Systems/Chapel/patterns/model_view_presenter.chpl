module PatternModelViewPresenter {
  proc run(): string {
    const modelReady = true;
    const presenterReady = modelReady;
    assert(presenterReady);
    return "view=ready";
  }
}
