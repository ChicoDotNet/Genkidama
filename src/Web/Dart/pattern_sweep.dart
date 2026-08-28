typedef PatternCheck = bool Function();

abstract interface class Expression {
  int evaluate();
}

final class Literal implements Expression {
  const Literal(this.value);
  final int value;

  @override
  int evaluate() => value;
}

final class Add implements Expression {
  const Add(this.left, this.right);
  final Expression left;
  final Expression right;

  @override
  int evaluate() => left.evaluate() + right.evaluate();
}

final class Multiply implements Expression {
  const Multiply(this.left, this.right);
  final Expression left;
  final Expression right;

  @override
  int evaluate() => left.evaluate() * right.evaluate();
}

bool commandExample() {
  final commands = <int>[10, -3];
  var balance = 0;
  for (final command in commands) {
    balance += command;
  }
  final undone = balance - commands.last;
  return balance == 7 && undone == 10;
}

bool interpreterExample() =>
    const Add(Literal(2), Multiply(Literal(3), Literal(4))).evaluate() == 14;

bool iteratorExample() {
  final values = <int>[10, 20];
  var cursor = 0;
  int? next() => cursor < values.length ? values[cursor++] : null;
  return next() == 10 && cursor == 1 && next() == 20 && next() == null;
}

bool mediatorExample() {
  String mediate(String sender, String message) =>
      sender == 'sales' ? 'billing:$message' : 'sales:$message';
  return mediate('sales', 'invoice') == 'billing:invoice';
}

bool mementoExample() {
  var current = 'v1';
  final snapshot = current;
  current = 'v2';
  current = snapshot;
  return current == 'v1';
}

bool observerExample() {
  final observed = <String>[];
  final observers = <void Function(int)>[
    (value) => observed.add('audit:$value'),
    (value) => observed.add('ui:$value'),
  ];
  for (final observer in observers) {
    observer(7);
  }
  return observed.join('|') == 'audit:7|ui:7';
}

bool stateExample() {
  var loggedIn = false;
  String toggle() {
    loggedIn = !loggedIn;
    return loggedIn ? 'login' : 'logout';
  }

  return toggle() == 'login' && toggle() == 'logout' && !loggedIn;
}

bool strategyExample() {
  int price(int Function(int) strategy) => strategy(100);
  return price((value) => value) == 100 &&
      price((value) => value * 80 ~/ 100) == 80;
}

bool templateMethodExample() {
  String run(String input, String Function(String) transform) =>
      'open|${transform(input)}|close';
  return run('abc', (value) => value.split('').reversed.join()) ==
      'open|cba|close';
}

bool visitorExample() {
  final shapes = <({int area, int perimeter})>[
    (area: 12, perimeter: 12),
    (area: 12, perimeter: 14),
  ];
  int visitArea(({int area, int perimeter}) shape) => shape.area;
  int visitPerimeter(({int area, int perimeter}) shape) => shape.perimeter;
  return visitArea(shapes.first) == 12 && visitPerimeter(shapes.last) == 14;
}

bool mvcExample() {
  var model = 3;
  void controller() => model++;
  String view() => 'count=$model';
  controller();
  return view() == 'count=4';
}

bool mvvmExample() {
  ({String greeting, String state}) viewModel(String name, bool enabled) => (
        greeting: 'Hello $name',
        state: enabled ? 'enabled' : 'disabled',
      );
  final vm = viewModel('Ada', true);
  return vm.greeting == 'Hello Ada' && vm.state == 'enabled';
}

bool microkernelExample() {
  final plugins = <String, int Function(int)>{
    'double': (value) => value * 2,
    'square': (value) => value * value,
  };
  return plugins['double']!(5) == 10 && plugins['square']!(3) == 9;
}

bool microservicesExample() {
  int inventory(String sku) => sku == 'A' ? 3 : 0;
  int pricing(String sku) => sku == 'A' ? 20 : 0;
  return inventory('A') == 3 && pricing('A') == 20;
}

bool enterpriseAdapterExample() {
  int legacy(int cents) => cents;
  int adapt(int dollars) => legacy(dollars * 100);
  return adapt(12) == 1200;
}

bool enterpriseBridgeExample() {
  String render(String Function(String) transport, String payload) =>
      transport(payload);
  return render((payload) => 'http:$payload', 'x') == 'http:x' &&
      render((payload) => 'queue:$payload', 'x') == 'queue:x';
}

bool enterpriseFacadeExample() {
  bool validate(int value) => value > 0;
  String persist(int value) => 'saved:$value';
  String facade(int value) => validate(value) ? persist(value) : 'rejected';
  return facade(5) == 'saved:5' && facade(0) == 'rejected';
}

bool brokerExample() {
  final registry = <String, int Function(int)>{
    'tax': (value) => value * 16 ~/ 100,
  };
  return registry['tax']!(100) == 16;
}

bool messageBusExample() {
  final delivered = <String>[];
  final subscribers = <void Function(String)>[
    (message) => delivered.add('audit:$message'),
    (message) => delivered.add('mail:$message'),
  ];
  for (final subscriber in subscribers) {
    subscriber('paid');
  }
  return delivered.join('|') == 'audit:paid|mail:paid';
}

bool serviceLocatorExample() {
  final services = <String, String>{'clock': '12:00', 'region': 'mx'};
  return services['region'] == 'mx';
}

bool activeObjectExample() {
  final queue = <String>[];
  queue.add('sync');
  final ran = 'run:${queue.removeAt(0)}';
  return ran == 'run:sync' && queue.isEmpty;
}

bool monitorObjectExample() {
  var balance = 5;
  void deposit(int amount) => balance += amount;
  bool withdraw(int amount) {
    if (balance < amount) return false;
    balance -= amount;
    return true;
  }

  deposit(10);
  return withdraw(7) && balance == 8;
}

bool halfSyncHalfAsyncExample() {
  final asyncQueue = <String>[];
  asyncQueue.add('evt');
  final processed = 'processed:${asyncQueue.removeAt(0)}';
  return processed == 'processed:evt' && asyncQueue.isEmpty;
}

bool leaderFollowersExample() {
  final pool = <String>['a', 'b', 'c'];
  final leader = pool.removeAt(0);
  pool.add(leader);
  return '$leader:evt' == 'a:evt' && pool.join(',') == 'b,c,a';
}

bool clientServerExample() {
  String server(String request) => 'response($request)';
  String client(String request) => server(request);
  return client('ping') == 'response(ping)';
}

bool peerToPeerExample() {
  String send(String from, String to, String payload) => '$from->$to:$payload';
  return send('a', 'b', 'x') == 'a->b:x' &&
      send('b', 'a', 'y') == 'b->a:y';
}

bool publishSubscribeExample() {
  final subscriptions = <String, List<String>>{
    'orders': <String>['audit', 'warehouse'],
    'users': <String>['crm'],
  };
  return subscriptions['orders']!.join(',') == 'audit,warehouse';
}

bool distributedProxyExample() {
  String remote(int id) => 'remote-user-$id';
  String proxy(int id) => remote(id);
  return proxy(7) == 'remote-user-7';
}

bool presentationAbstractionControlExample() {
  var abstraction = 4;
  void control(String action) {
    if (action == 'inc') abstraction++;
  }

  String presentation() => 'value=$abstraction';
  control('inc');
  return presentation() == 'value=5';
}

bool modelViewPresenterExample() {
  String presenter(String value) => 'Hello $value';
  String passiveView(String text) => '[$text]';
  return passiveView(presenter('Ada')) == '[Hello Ada]';
}

bool documentViewExample() {
  const document = 'hello';
  String plain(String value) => value;
  String upper(String value) => value.toUpperCase();
  return plain(document) == 'hello' && upper(document) == 'HELLO';
}

bool activeRecordExample() {
  final store = <int, String>{};
  void save(int id, String name) => store[id] = name;
  save(1, 'Ada');
  return store[1] == 'Ada';
}

bool dataMapperExample() {
  ({int id, String name}) toRow(({int id, String name}) record) => record;
  ({int id, String name}) fromRow(({int id, String name}) row) => row;
  final row = toRow((id: 1, name: 'Ada'));
  return fromRow(row) == (id: 1, name: 'Ada');
}

bool unitOfWorkExample() {
  final pending = <({int id, String name})>[];
  final store = <({int id, String name})>[];
  pending.add((id: 1, name: 'Ada'));
  store.addAll(pending);
  pending.clear();
  return store.length == 1 && store.single.name == 'Ada' && pending.isEmpty;
}

bool repositoryExample() {
  final store = <int, String>{};
  void save(int id, String name) => store[id] = name;
  String? find(int id) => store[id];
  save(1, 'Ada');
  return find(1) == 'Ada';
}

bool dependencyInjectionExample() {
  String service(String Function() clock) => 'time=${clock()}';
  return service(() => '12:00') == 'time=12:00';
}

bool lazyInitializationExample() {
  String? resource;
  var created = 0;
  String getResource() {
    if (resource == null) {
      resource = 'resource';
      created++;
    }
    return resource!;
  }

  return getResource() == 'resource' &&
      getResource() == 'resource' &&
      created == 1;
}

bool objectPoolExample() {
  final pool = <String>['c1', 'c2'];
  final resource = pool.removeAt(0);
  pool.add(resource);
  return pool.join(',') == 'c2,c1';
}

bool nullObjectExample() {
  String run(String Function(String) logger, String message) => logger(message);
  return run((message) => 'log:$message', 'x') == 'log:x' &&
      run((_) => '', 'x').isEmpty;
}

void main() {
  final tests = <String, PatternCheck>{
    'Command': commandExample,
    'Interpreter': interpreterExample,
    'Iterator': iteratorExample,
    'Mediator': mediatorExample,
    'Memento': mementoExample,
    'Observer': observerExample,
    'State': stateExample,
    'Strategy': strategyExample,
    'Template Method': templateMethodExample,
    'Visitor': visitorExample,
    'MVC': mvcExample,
    'MVVM': mvvmExample,
    'Microkernel': microkernelExample,
    'Microservices': microservicesExample,
    'Enterprise Adapter': enterpriseAdapterExample,
    'Enterprise Bridge': enterpriseBridgeExample,
    'Enterprise Facade': enterpriseFacadeExample,
    'Broker': brokerExample,
    'Message Bus': messageBusExample,
    'Service Locator': serviceLocatorExample,
    'Active Object': activeObjectExample,
    'Monitor Object': monitorObjectExample,
    'Half-Sync / Half-Async': halfSyncHalfAsyncExample,
    'Leader / Followers': leaderFollowersExample,
    'Client-Server': clientServerExample,
    'Peer-to-Peer': peerToPeerExample,
    'Publish-Subscribe': publishSubscribeExample,
    'Distributed Proxy': distributedProxyExample,
    'Presentation-Abstraction-Control': presentationAbstractionControlExample,
    'Model-View-Presenter': modelViewPresenterExample,
    'Document-View': documentViewExample,
    'Active Record': activeRecordExample,
    'Data Mapper': dataMapperExample,
    'Unit of Work': unitOfWorkExample,
    'Repository': repositoryExample,
    'Dependency Injection': dependencyInjectionExample,
    'Lazy Initialization': lazyInitializationExample,
    'Object Pool': objectPoolExample,
    'Null Object': nullObjectExample,
  };

  final failed = <String>[];
  for (final entry in tests.entries) {
    if (!entry.value()) failed.add(entry.key);
  }
  if (failed.isNotEmpty) {
    throw StateError('Dart pattern sweep failures: ${failed.join(', ')}');
  }
  print('Dart pattern sweep: ${tests.length}/${tests.length} examples passed');
}
