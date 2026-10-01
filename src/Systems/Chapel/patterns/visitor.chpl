module PatternVisitor {
  proc run(): string {
    const width = 3;
    const height = 6;
    const area = width * height;
    assert(area == 18);
    return "area=18";
  }
}
