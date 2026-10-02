module PatternMicrokernel {
  proc run(): string {
    const plugin = "square";
    const value = 4;
    const result = if plugin == "square" then value * value else value;
    assert(result == 16);
    return "plugin=16";
  }
}
