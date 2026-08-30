final class CheckoutMediator {
    private final Map<String, Closure<Void>> colleagues = [:]

    void register(String name, Closure<Void> receive) {
        colleagues[name] = receive
    }

    void send(String sender, String recipient, String message) {
        Closure<Void> receive = colleagues[recipient]
        assert receive != null: "unknown colleague: ${recipient}"
        receive.call(sender, message)
    }
}

final List<String> events = []
final CheckoutMediator mediator = new CheckoutMediator()
mediator.register('inventory') { String sender, String message ->
    events << "inventory<-${sender}:${message}"
}
mediator.register('payment') { String sender, String message ->
    events << "payment<-${sender}:${message}"
}

mediator.send('payment', 'inventory', 'paid')
mediator.send('inventory', 'payment', 'reserved')

assert events == [
    'inventory<-payment:paid',
    'payment<-inventory:reserved'
]
