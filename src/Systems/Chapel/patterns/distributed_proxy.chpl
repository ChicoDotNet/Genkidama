module PatternDistributedProxy {
  proc run(): string {
    const remoteValue = 42;
    const proxyValue = remoteValue;
    assert(proxyValue == 42);
    return "remote=42";
  }
}
