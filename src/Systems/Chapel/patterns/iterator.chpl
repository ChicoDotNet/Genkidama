module PatternIterator {
  proc run(): string {
    var sum = 0;
    for value in 1..3 do sum += value;
    assert(sum == 6);
    return "sum=6";
  }
}
