use PatternLeaderFollowers;

proc main() {
  const actual = run();
  assert(actual == "leader=1;handled=2");
  writeln("chapel-cell-pass: leader_followers");
}
