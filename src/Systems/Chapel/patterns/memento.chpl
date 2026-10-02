module PatternMemento {
  proc run(): string {
    var current = "draft";
    const snapshot = current;
    current = "published";
    current = snapshot;
    assert(current == "draft");
    return "restored=draft";
  }
}
