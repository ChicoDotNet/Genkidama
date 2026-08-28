import std/strutils

type
  Handler = ref object
    name: string
    limit: int
    next: Handler

proc handle(handler: Handler, amount: int, visited: var seq[string]): string =
  visited.add(handler.name)
  if amount <= handler.limit:
    return "handled=" & handler.name & ";result=refund(" & $amount & ")"
  if handler.next.isNil:
    return "handled=none;result=rejected"
  handler.next.handle(amount, visited)

let escalation = Handler(name: "escalation", limit: high(int))
let billing = Handler(name: "billing", limit: 500, next: escalation)
let faq = Handler(name: "faq", limit: 50, next: billing)
var visited: seq[string] = @[]
let result = faq.handle(250, visited)
echo "visited=" & visited.join(">") & ";" & result
