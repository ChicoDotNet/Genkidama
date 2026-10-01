module PatternComposite {
  proc run(): string {
    const leaf = 2;
    const docs = 8;
    const root = leaf + docs;
    assert(root == 10);
    return "root=10";
  }
}
