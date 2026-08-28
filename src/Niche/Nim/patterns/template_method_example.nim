proc run*(): bool =
  proc pipeline(readStep: string, transform: proc(): string): string = readStep & ">" & transform() & ">publish"
  pipeline("read-csv", proc(): string = "normalize") == "read-csv>normalize>publish"
