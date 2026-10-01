module PatternFactoryMethod {
  proc run(): string {
    const requested = "postgresql";
    const connection = if requested == "postgresql" then "postgresql" else "mysql";
    assert(connection == "postgresql");
    return "postgresql=connected";
  }
}
