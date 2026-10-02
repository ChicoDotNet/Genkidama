module PatternMvc {
  proc run(): string {
    const model = 5;
    const controllerProjection = model;
    const view = controllerProjection;
    assert(view == 5);
    return "view=5";
  }
}
