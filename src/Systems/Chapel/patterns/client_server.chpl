module PatternClientServer {
  proc run(): string {
    const request = "ping";
    const response = if request == "ping" then "pong" else "error";
    assert(response == "pong");
    return "response=pong";
  }
}
