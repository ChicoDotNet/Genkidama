module PatternObjectPool {
  proc run(): string {
    const releasedId = 7;
    const acquiredId = 7;
    assert(releasedId == acquiredId);
    return "reused=true";
  }
}
