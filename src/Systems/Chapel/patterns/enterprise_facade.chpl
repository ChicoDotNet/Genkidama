module PatternEnterpriseFacade {
  proc run(): string {
    const crmOk = true;
    const erpOk = true;
    assert(crmOk && erpOk);
    return "customer=ok";
  }
}
