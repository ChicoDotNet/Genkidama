module PatternMediator {
  proc run(): string {
    const sender = "alice";
    const receiver = "bob";
    const message = "hello";
    assert(sender != receiver && message == "hello");
    return "alice>bob=hello";
  }
}
