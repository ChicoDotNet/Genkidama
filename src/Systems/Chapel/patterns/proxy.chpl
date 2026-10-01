module PatternProxy {
  proc run(): string {
    var backendFetches = 0;
    var cached = false;
    if !cached {
      backendFetches += 1;
      cached = true;
    }
    if !cached then backendFetches += 1;
    assert(backendFetches == 1);
    return "fetches=1";
  }
}
