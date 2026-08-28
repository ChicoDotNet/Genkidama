proc run*(): bool =
  var state = "draft"
  let snapshot = state
  state = "published"
  let published = state == "published"
  state = snapshot
  published and state == "draft"
