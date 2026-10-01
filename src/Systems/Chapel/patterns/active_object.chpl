module PatternActiveObject {
  proc run(): string {
    const queuedValue = 3;
    const result = queuedValue * queuedValue;
    assert(result == 9);
    return "result=9";
  }
}
