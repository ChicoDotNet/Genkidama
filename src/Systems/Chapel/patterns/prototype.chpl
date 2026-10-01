module PatternPrototype {
  proc run(): string {
    const original = "metrics";
    const clone = original + ",tracing";
    assert(original == "metrics");
    assert(clone == "metrics,tracing");
    return "clone=tracing";
  }
}
