module PatternMonitorObject {
  proc run(): string {
    var count = 0;
    count += 1;
    count += 4;
    assert(count == 5);
    return "count=5";
  }
}
