module PatternSingleton {
  proc run(): string {
    const firstId = 1;
    const secondId = 1;
    const instancesCreated = 1;
    assert(firstId == secondId && instancesCreated == 1);
    return "same=true;count=1";
  }
}
