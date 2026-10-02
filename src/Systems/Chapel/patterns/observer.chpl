module PatternObserver {
  proc run(): string {
    const subscribers = 2;
    var delivered = 0;
    for i in 1..subscribers do delivered += 1;
    assert(delivered == 2);
    return "events=2";
  }
}
