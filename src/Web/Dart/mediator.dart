typedef Receiver = void Function(String source, String event);

class CheckoutMediator {
  final Map<String, Receiver> _colleagues = <String, Receiver>{};

  void register(String name, Receiver receiver) {
    _colleagues[name] = receiver;
  }

  void route(String source, String target, String event) {
    final receiver = _colleagues[target];
    if (receiver == null) {
      throw StateError('UnknownColleague:$target');
    }
    receiver(source, event);
  }
}

class CheckoutColleague {
  CheckoutColleague(this.name, this.mediator);

  final String name;
  final CheckoutMediator mediator;
  final List<String> inbox = <String>[];

  void send(String target, String event) {
    mediator.route(name, target, event);
  }

  void receive(String source, String event) {
    inbox.add('$source>$name:$event');
  }
}

void verifyMediator() {
  final mediator = CheckoutMediator();
  final payment = CheckoutColleague('payment', mediator);
  final inventory = CheckoutColleague('inventory', mediator);

  mediator
    ..register(payment.name, payment.receive)
    ..register(inventory.name, inventory.receive);

  payment.send('inventory', 'reserve-order-42');
  inventory.send('payment', 'stock-reserved-order-42');

  if (inventory.inbox.join('>') !=
      'payment>inventory:reserve-order-42') {
    throw StateError('payment-to-inventory routing failed');
  }
  if (payment.inbox.join('>') !=
      'inventory>payment:stock-reserved-order-42') {
    throw StateError('inventory-to-payment routing failed');
  }

  var unknownColleagueObserved = false;
  try {
    payment.send('shipping', 'prepare-order-42');
  } on StateError catch (error) {
    unknownColleagueObserved = error.message == 'UnknownColleague:shipping';
  }
  if (!unknownColleagueObserved) {
    throw StateError('unknown colleague failure contract was not observed');
  }
}

void main() {
  verifyMediator();
  print('Dart Mediator: passed');
}
