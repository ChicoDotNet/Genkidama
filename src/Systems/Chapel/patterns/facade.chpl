module PatternFacade {
  proc run(): string {
    const authenticated = true;
    const reserved = true;
    const charged = true;
    assert(authenticated && reserved && charged);
    return "checkout=charged";
  }
}
