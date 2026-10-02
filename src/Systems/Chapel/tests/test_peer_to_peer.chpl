use PatternPeerToPeer;

proc main() {
  const actual = run();
  assert(actual == "peers=3");
  writeln("chapel-cell-pass: peer_to_peer");
}
