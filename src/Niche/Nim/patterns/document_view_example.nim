proc run*(): bool =
  let title = "Final"
  let words = 120
  let editor = "editor:" & title & ":" & $words
  let summary = "summary:" & title
  editor == "editor:Final:120" and summary == "summary:Final"
