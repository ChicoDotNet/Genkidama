module PatternEnterpriseAdapter {
  proc run(): string {
    const legacyCustomerId = 42;
    const normalizedCustomerId = legacyCustomerId;
    assert(normalizedCustomerId == 42);
    return "erp=42";
  }
}
