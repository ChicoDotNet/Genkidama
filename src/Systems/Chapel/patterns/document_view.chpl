module PatternDocumentView {
  proc run(): string {
    const document = 1;
    const views = document + 1;
    assert(views == 2);
    return "views=2";
  }
}
