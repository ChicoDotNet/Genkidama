module PatternNullObject {
  proc run(): string {
    const notifications = 0;
    assert(notifications == 0);
    return "messages=0";
  }
}
