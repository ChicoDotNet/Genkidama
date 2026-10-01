module PatternEnterpriseBridge {
  proc run(): string {
    const sourceSystem = "sap";
    const targetFormat = "json";
    assert(sourceSystem == "sap" && targetFormat == "json");
    return "sap>json";
  }
}
