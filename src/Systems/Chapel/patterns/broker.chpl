module PatternBroker {
  proc run(): string {
    const queueDepth = 2;
    const route = if queueDepth > 1 then "worker-b" else "worker-a";
    assert(route == "worker-b");
    return "route=worker-b";
  }
}
