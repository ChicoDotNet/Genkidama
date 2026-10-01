module PatternState {
  proc run(): string {
    const before = "locked";
    const event = "coin";
    const after = if before == "locked" && event == "coin" then "unlocked" else before;
    assert(after == "unlocked");
    return "locked>unlocked";
  }
}
