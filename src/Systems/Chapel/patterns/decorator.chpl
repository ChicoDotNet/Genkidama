module PatternDecorator {
  proc run(): string {
    const base = "alert";
    const encrypted = "enc(" + base + ")";
    const audited = "audit(" + encrypted + ")";
    assert(audited == "audit(enc(alert))");
    return audited;
  }
}
