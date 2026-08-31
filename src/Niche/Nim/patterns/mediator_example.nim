import std/tables

type Receiver = proc(sender, message: string): string

type CheckoutMediator = object
  colleagues: Table[string, Receiver]

proc paymentReceiver(sender, message: string): string =
  sender & ":" & message & "->payment"

proc inventoryReceiver(sender, message: string): string =
  sender & ":" & message & "->inventory"

proc register(mediator: var CheckoutMediator; name: string; receiver: Receiver) =
  mediator.colleagues[name] = receiver

proc send(mediator: CheckoutMediator; sender, recipient, message: string): string =
  if not mediator.colleagues.hasKey(recipient):
    raise newException(ValueError, "unknown colleague: " & recipient)
  mediator.colleagues[recipient](sender, message)

proc run*(): bool =
  var mediator = CheckoutMediator(colleagues: initTable[string, Receiver]())
  mediator.register("payment", paymentReceiver)
  mediator.register("inventory", inventoryReceiver)

  if mediator.send("payment", "inventory", "paid") != "payment:paid->inventory":
    return false
  if mediator.send("inventory", "payment", "reserved") != "inventory:reserved->payment":
    return false

  try:
    discard mediator.send("payment", "shipping", "dispatch")
    return false
  except ValueError as error:
    return error.msg == "unknown colleague: shipping"
