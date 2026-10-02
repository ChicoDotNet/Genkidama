module PatternPeerToPeer {
  proc run(): string {
    const peers = 3;
    assert(peers >= 2);
    return "peers=3";
  }
}
