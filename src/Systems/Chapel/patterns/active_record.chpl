module PatternActiveRecord {
  proc run(): string {
    const rowId = 42;
    const savedId = rowId;
    assert(savedId == 42);
    return "saved=42";
  }
}
