import std/strutils
proc run*(): bool =
  let workers = @["worker-1", "worker-2", "worker-3"]
  let events = @["a", "b", "c"]
  var handled: seq[string] = @[]
  for index, event in events: handled.add(workers[index mod workers.len] & ":" & event)
  handled.join(">") == "worker-1:a>worker-2:b>worker-3:c" and workers[events.len mod workers.len] == "worker-1"
