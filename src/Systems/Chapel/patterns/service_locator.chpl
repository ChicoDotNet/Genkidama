module PatternServiceLocator {
  proc run(): string {
    const key = "mail";
    const located = if key == "mail" then "mail" else "none";
    assert(located == "mail");
    return "service=mail";
  }
}
