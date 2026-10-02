module PatternChainOfResponsibility {
  proc run(): string {
    const amount = 250;
    const handler = if amount >= 200 then "billing" else "faq";
    assert(handler == "billing");
    return "handled=billing";
  }
}
