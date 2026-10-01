module PatternDataMapper {
  proc run(): string {
    const databaseId = 42;
    const entityId = databaseId;
    assert(entityId == 42);
    return "entity=42";
  }
}
