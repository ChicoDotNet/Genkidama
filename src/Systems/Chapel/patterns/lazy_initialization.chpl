module PatternLazyInitialization {
  proc run(): string {
    var created = 0;
    var initialized = false;
    if !initialized {
      created += 1;
      initialized = true;
    }
    if !initialized then created += 1;
    assert(created == 1);
    return "created=1";
  }
}
