proc run*(): bool =
  proc view(name: string, value: int): string = name & ":view=" & $value
  view("child", 42) == "child:view=42" and view("root", 42) == "root:view=42"
