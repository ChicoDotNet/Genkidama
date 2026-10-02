module PatternCommand {
  proc run(): string {
    const initial = 10;
    const afterExecute = initial + 5;
    const afterUndo = afterExecute - 5;
    assert(afterExecute == 15 && afterUndo == 10);
    return "balance=15;undo=10";
  }
}
