module PatternPresentationAbstractionControl {
  proc run(): string {
    const presentationAgent = 1;
    const abstractionAgent = 1;
    assert(presentationAgent + abstractionAgent == 2);
    return "agents=2";
  }
}
