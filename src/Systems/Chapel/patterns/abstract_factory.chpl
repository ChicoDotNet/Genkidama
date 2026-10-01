module PatternAbstractFactory {
  proc run(): string {
    const theme = "dark";
    const button = if theme == "dark" then "button" else "button";
    const checkbox = if theme == "dark" then "checkbox" else "checkbox";
    assert(button == "button" && checkbox == "checkbox");
    return "dark=button+checkbox";
  }
}
