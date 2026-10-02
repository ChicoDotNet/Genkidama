module PatternInterpreter {
  proc run(): string {
    const result = 1 + 2 * 3;
    assert(result == 7);
    return "result=7";
  }
}
