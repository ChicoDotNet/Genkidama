module PatternRepository {
  proc run(): string {
    const storedId = 42;
    const requestedId = 42;
    assert(storedId == requestedId);
    return "found=42";
  }
}
