module PatternDependencyInjection {
  proc run(): string {
    const value = 4;
    const injectedMultiplier = 2;
    const result = value * injectedMultiplier;
    assert(result == 8);
    return "result=8";
  }
}
