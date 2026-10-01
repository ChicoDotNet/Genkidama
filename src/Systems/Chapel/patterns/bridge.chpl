module PatternBridge {
  proc run(): string {
    const device = "tv";
    const action = "muted";
    assert(device == "tv" && action == "muted");
    return device + ":" + action;
  }
}
