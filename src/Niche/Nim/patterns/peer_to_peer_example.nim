import std/strutils
proc run*(): bool =
  var inbox: seq[string] = @[]
  proc send(fromPeer, toPeer, data: string) = inbox.add(fromPeer & ">" & toPeer & ":" & data)
  send("peer-a", "peer-b", "block-42"); send("peer-a", "peer-c", "block-42")
  inbox.join(">") == "peer-a>peer-b:block-42>peer-a>peer-c:block-42"
