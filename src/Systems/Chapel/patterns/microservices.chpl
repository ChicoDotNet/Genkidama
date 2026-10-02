module PatternMicroservices {
  proc run(): string {
    const inventoryAccepted = true;
    const paymentAccepted = true;
    assert(inventoryAccepted && paymentAccepted);
    return "order=paid";
  }
}
