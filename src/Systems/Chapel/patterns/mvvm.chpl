module PatternMvvm {
  proc run(): string {
    const model = 5;
    const viewModelLabel = model;
    assert(viewModelLabel == 5);
    return "label=5";
  }
}
