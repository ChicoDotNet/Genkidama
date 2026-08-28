proc run*(): bool =
  proc service(clock: proc(): string): string = "at:" & clock()
  service(proc(): string = "10:00") == "at:10:00"
