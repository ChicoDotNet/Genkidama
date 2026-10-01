module PatternAdapter {
  proc run(): string {
    const fahrenheit = 212;
    const celsius = (fahrenheit - 32) * 5 / 9;
    assert(celsius == 100);
    return "celsius=100";
  }
}
