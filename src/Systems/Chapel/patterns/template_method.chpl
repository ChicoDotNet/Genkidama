module PatternTemplateMethod {
  proc run(): string {
    const first = "validate";
    const second = "persist";
    assert(first == "validate" && second == "persist");
    return "steps=validate>persist";
  }
}
