class CheckoutMediator:
    def __init__(self):
        self._routes = {}

    def register(self, name, receiver):
        self._routes[name] = receiver

    def send(self, sender, recipient, message):
        receiver = self._routes.get(recipient)
        if receiver is None:
            raise ValueError("UnknownColleague:%s" % recipient)
        return receiver(sender, message)


events = []


def inventory_receive(sender, message):
    event = "inventory<-%s:%s" % (sender, message)
    events.append(event)
    return event


def payment_receive(sender, message):
    event = "payment<-%s:%s" % (sender, message)
    events.append(event)
    return event


mediator = CheckoutMediator()
mediator.register("inventory", inventory_receive)
mediator.register("payment", payment_receive)

assert mediator.send("payment", "inventory", "paid") == "inventory<-payment:paid"
assert mediator.send("inventory", "payment", "reserved") == "payment<-inventory:reserved"
assert events == ["inventory<-payment:paid", "payment<-inventory:reserved"]

try:
    mediator.send("payment", "shipping", "dispatch")
    raise AssertionError("unknown colleague was accepted")
except ValueError as error:
    assert str(error) == "UnknownColleague:shipping"

print("MicroPython Mediator: passed")
