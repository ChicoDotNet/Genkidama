module PatternHalfSyncHalfAsync {
  proc run(): string {
    const asyncAccepted = 3;
    var syncHandled = 0;
    for i in 1..asyncAccepted do syncHandled += 1;
    assert(syncHandled == 3);
    return "handled=3";
  }
}
