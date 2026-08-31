extends SceneTree

var events: Array[String] = []
var routes: Dictionary = {}

func _init() -> void:
    routes = {
        "inventory": Callable(self, "inventory_receive"),
        "payment": Callable(self, "payment_receive")
    }

    assert(mediate("payment", "inventory", "paid"))
    assert(mediate("inventory", "payment", "reserved"))
    assert(not mediate("payment", "shipping", "dispatch"))
    assert(events == ["inventory<-payment:paid", "payment<-inventory:reserved"])

    print("GDScript Mediator: passed")
    quit()

func mediate(sender: String, recipient: String, message: String) -> bool:
    if not routes.has(recipient):
        return false
    var receiver: Callable = routes[recipient]
    receiver.call(sender, message)
    return true

func inventory_receive(sender: String, message: String) -> void:
    events.append("inventory<-%s:%s" % [sender, message])

func payment_receive(sender: String, message: String) -> void:
    events.append("payment<-%s:%s" % [sender, message])
