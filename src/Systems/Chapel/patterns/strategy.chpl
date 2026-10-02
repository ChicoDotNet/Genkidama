module PatternStrategy {
  proc run(): string {
    const value = 4;
    const strategy = "square";
    const result = if strategy == "square" then value * value else value + value;
    assert(result == 16);
    return "square=16";
  }
}
