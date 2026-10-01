module PatternUnitOfWork {
  proc run(): string {
    const inserts = 1;
    const updates = 1;
    const commits = inserts + updates;
    assert(commits == 2);
    return "commits=2";
  }
}
