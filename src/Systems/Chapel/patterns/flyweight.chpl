module PatternFlyweight {
  proc run(): string {
    const styleA = 1;
    const styleB = 1;
    const styleC = 2;
    assert(styleA == styleB && styleA != styleC);
    return "styles=2;shared=true";
  }
}
