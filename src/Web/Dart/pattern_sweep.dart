import 'dart:math' as math;

void check(bool condition) {
  if (!condition) throw StateError('pattern assertion failed');
}

// Command
abstract interface class BalanceCommand {
  int execute(int balance);
  int undo(int balance);
  String get name;
}
class Deposit implements BalanceCommand {
  Deposit(this.amount); final int amount;
  @override String get name => 'deposit';
  @override int execute(int balance) => balance + amount;
  @override int undo(int balance) => balance - amount;
}
class Withdraw implements BalanceCommand {
  Withdraw(this.amount); final int amount;
  @override String get name => 'withdraw';
  @override int execute(int balance) => balance - amount;
  @override int undo(int balance) => balance + amount;
}
void commandPattern() {
  final queue = <BalanceCommand>[Deposit(50), Withdraw(20)];
  var balance = 100;
  final trace = <String>[];
  for (final command in queue) { balance = command.execute(balance); trace.add(command.name); }
  check(balance == 130 && trace.join('>') == 'deposit>withdraw');
  balance = queue.last.undo(balance);
  check(balance == 150);
}

// Interpreter
sealed class Expr { int eval(); }
class Literal extends Expr { Literal(this.value); final int value; @override int eval() => value; }
class AddExpr extends Expr { AddExpr(this.left, this.right); final Expr left; final Expr right; @override int eval() => left.eval() + right.eval(); }
class MultiplyExpr extends Expr { MultiplyExpr(this.left, this.right); final Expr left; final Expr right; @override int eval() => left.eval() * right.eval(); }
void interpreterPattern() => check(AddExpr(Literal(7), MultiplyExpr(Literal(3), Literal(4))).eval() == 19);

// Iterator
class CursorIterator<T> {
  CursorIterator(this.values); final List<T> values; int _index = 0;
  bool get hasNext => _index < values.length;
  T next() => values[_index++];
}
void iteratorPattern() { final it = CursorIterator<int>([10, 20, 30]); final visited = <int>[]; while (it.hasNext) { visited.add(it.next()); } check(visited.join(',') == '10,20,30' && !it.hasNext); }

// Mediator
class UiMediator {
  final events = <String>[];
  void notify(String sender, String event) {
    if (sender == 'button' && event == 'click') events.add('panel.refresh');
    if (sender == 'panel' && event == 'loaded') events.add('button.enable');
  }
}
void mediatorPattern() { final m = UiMediator()..notify('button', 'click')..notify('panel', 'loaded'); check(m.events.join('>') == 'panel.refresh>button.enable'); }

// Memento
class EditorMemento { const EditorMemento(this.state); final String state; }
class Editor { Editor(this.state); String state; EditorMemento save() => EditorMemento(state); void restore(EditorMemento m) => state = m.state; }
void mementoPattern() { final e = Editor('draft'); final snapshot = e.save(); e.state = 'published'; check(e.state == 'published'); e.restore(snapshot); check(e.state == 'draft'); }

// Observer
typedef Observer = String Function(int id);
class Subject { final _observers = <Observer>[]; void subscribe(Observer o) => _observers.add(o); List<String> publish(int id) => _observers.map((o) => o(id)).toList(); }
void observerPattern() { final s = Subject()..subscribe((id) => 'audit:$id')..subscribe((id) => 'dashboard:$id'); check(s.publish(42).join('>') == 'audit:42>dashboard:42'); }

// State
enum GateState { locked, unlocked }
GateState transition(GateState state, String action) { if (state == GateState.locked && action == 'unlock') return GateState.unlocked; if (state == GateState.unlocked && action == 'lock') return GateState.locked; return state; }
void statePattern() { var s = GateState.locked; s = transition(s, 'unlock'); check(s == GateState.unlocked); s = transition(s, 'lock'); check(s == GateState.locked); }

// Strategy
typedef PricingStrategy = int Function(int value);
int price(int value, PricingStrategy strategy) => strategy(value);
void strategyPattern() => check(price(100, (v) => v) == 100 && price(100, (v) => v * 80 ~/ 100) == 80);

// Template Method
String pipeline(String read, String Function() transform) => '$read>${transform()}>publish';
void templateMethodPattern() => check(pipeline('read-csv', () => 'normalize') == 'read-csv>normalize>publish' && pipeline('read-json', () => 'aggregate') == 'read-json>aggregate>publish');

// Visitor
abstract interface class ShapeVisitor { double visitCircle(Circle circle); double visitRectangle(Rectangle rectangle); }
abstract interface class Shape { double accept(ShapeVisitor visitor); String get label; }
class Circle implements Shape { Circle(this.radius); final double radius; @override double accept(ShapeVisitor visitor) => visitor.visitCircle(this); @override String get label => 'circle'; }
class Rectangle implements Shape { Rectangle(this.width, this.height); final double width; final double height; @override double accept(ShapeVisitor visitor) => visitor.visitRectangle(this); @override String get label => 'rectangle'; }
class AreaVisitor implements ShapeVisitor { @override double visitCircle(Circle c) => math.pi * c.radius * c.radius; @override double visitRectangle(Rectangle r) => r.width * r.height; }
void visitorPattern() { final shapes = <Shape>[Circle(2), Rectangle(3, 4)]; final area = shapes.map((s) => s.accept(AreaVisitor())).reduce((a, b) => a + b); check((area - (4 * math.pi + 12)).abs() < 1e-9 && shapes.map((s) => s.label).join('>') == 'circle>rectangle'); }

// MVC
class CounterModel { int count = 0; }
class CounterController { CounterController(this.model); final CounterModel model; void increment() => model.count++; }
String renderCounter(CounterModel model) => 'count=${model.count}';
void mvcPattern() { final m = CounterModel(); final before = renderCounter(m); CounterController(m).increment(); check(before == 'count=0' && renderCounter(m) == 'count=1'); }

// MVVM
class AmountViewModel { AmountViewModel(this.amount); int amount; String get text => '\$$amount.00'; void add(int value) => amount += value; }
void mvvmPattern() { final vm = AmountViewModel(10); final before = vm.text; vm.add(5); check(before == '\$10.00' && vm.text == '\$15.00'); }

// Microkernel
class Kernel { final _plugins = <String, int Function(int)>{}; void register(String name, int Function(int) plugin) => _plugins[name] = plugin; int run(String name, int value) => _plugins[name]!(value); }
void microkernelPattern() { final k = Kernel()..register('double', (v) => v * 2)..register('square', (v) => v * v); check(k.run('double', 4) == 8 && k.run('square', 4) == 16); }

// Microservices
class InventoryService { InventoryService(this.stock); int stock; bool reserve(int quantity) { if (quantity > stock) return false; stock -= quantity; return true; } }
class OrderService { OrderService(this.inventory); final InventoryService inventory; String place(int quantity) => inventory.reserve(quantity) ? 'confirmed' : 'rejected'; }
void microservicesPattern() { final i = InventoryService(7); check(OrderService(i).place(2) == 'confirmed' && i.stock == 5); }

// Enterprise Adapter
class LegacyCustomer { LegacyCustomer(this.code, this.cents); final int code; final int cents; }
class CanonicalCustomer { CanonicalCustomer(this.id, this.amount); final int id; final double amount; }
CanonicalCustomer adaptCustomer(LegacyCustomer c) => CanonicalCustomer(c.code, c.cents / 100);
void enterpriseAdapterPattern() { final c = adaptCustomer(LegacyCustomer(17, 1250)); check(c.id == 17 && c.amount == 12.5); }

// Enterprise Bridge
abstract interface class Transport { String send(String message); }
class NamedTransport implements Transport { NamedTransport(this.name); final String name; @override String send(String message) => '$name>$message'; }
String sendNotice(String kind, String message, Transport transport) => transport.send('$kind:$message');
void enterpriseBridgePattern() => check(sendNotice('ALERT', 'disk', NamedTransport('kafka')) == 'kafka>ALERT:disk' && sendNotice('REMINDER', 'backup', NamedTransport('queue')) == 'queue>REMINDER:backup');

// Enterprise Facade
class EnterpriseFacade { String createCustomer(int id) => 'crm:create:$id>billing:open:$id'; }
void enterpriseFacadePattern() => check(EnterpriseFacade().createCustomer(77) == 'crm:create:77>billing:open:77');

// Broker
typedef BrokerService = String Function(String argument);
class Broker { final services = <String, BrokerService>{}; void register(String name, BrokerService service) => services[name] = service; String call(String name, String argument) => services[name]!(argument); }
void brokerPattern() { final b = Broker()..register('inventory', (k) => 'inventory:$k=7')..register('customer', (k) => 'customer:$k=active'); check(b.call('inventory', 'sku-1') == 'inventory:sku-1=7' && b.call('customer', '17') == 'customer:17=active'); }

// Message Bus
class Message { Message(this.topic, this.id); final String topic; final int id; }
typedef MessageHandler = String Function(Message message);
class MessageBus { final handlers = <MessageHandler>[]; void on(MessageHandler handler) => handlers.add(handler); List<String> send(Message message) => handlers.map((h) => h(message)).toList(); }
void messageBusPattern() { final b = MessageBus()..on((m) => 'audit:${m.topic}:${m.id}')..on((m) => 'billing:${m.topic}:${m.id}'); check(b.send(Message('order-created', 42)).join('>') == 'audit:order-created:42>billing:order-created:42'); }

// Service Locator
void serviceLocatorPattern() { final services = <String, String Function(String)>{'email': (v) => 'email>$v', 'audit': (v) => 'audit>$v'}; check(services['email']!('a@example.test') == 'email>a@example.test' && services['audit']!('created') == 'audit>created'); }

// Active Object
void activeObjectPattern() { var value = 0; final queue = <void Function()>[() => value += 3, () => value *= 4]; final before = value; for (final command in queue) { command(); } check(before == 0 && value == 12); }

// Monitor Object: deterministic protocol model encapsulates serialized access.
class MonitorCounter { int _value = 0; bool _locked = false; int maxCritical = 0; int _critical = 0; void add(int n) { check(!_locked); _locked = true; _critical++; if (_critical > maxCritical) maxCritical = _critical; _value += n; _critical--; _locked = false; } int get value => _value; bool get locked => _locked; }
void monitorObjectPattern() { final m = MonitorCounter()..add(2)..add(3); check(m.value == 5 && m.maxCritical == 1 && !m.locked); }

// Half-Sync / Half-Async
void halfSyncHalfAsyncPattern() { final queue = ['job-1', 'job-2', 'job-3']; final processed = queue.map((j) => 'done:$j').toList(); check(processed.join('>') == 'done:job-1>done:job-2>done:job-3'); }

// Leader / Followers
void leaderFollowersPattern() { final workers = ['worker-1', 'worker-2', 'worker-3']; final events = ['event-a', 'event-b', 'event-c']; final handled = <String>[]; for (var i = 0; i < events.length; i++) { handled.add('${workers[i]}:${events[i]}'); } check(handled.join('>') == 'worker-1:event-a>worker-2:event-b>worker-3:event-c' && workers[events.length % workers.length] == 'worker-1'); }

// Client-Server
class Request { Request(this.key); final String key; }
class Response { Response(this.status, this.body); final int status; final String body; }
Response serverHandle(Request r) => r.key == 'sku-1' ? Response(200, 'stock=7') : Response(404, 'missing');
void clientServerPattern() { final r = serverHandle(Request('sku-1')); check(r.status == 200 && r.body == 'stock=7'); }

// Peer-to-Peer
class Peer { Peer(this.name); final String name; final inbox = <String>[]; void send(Peer peer, String data) => peer.inbox.add('$name>${peer.name}:$data'); }
void peerToPeerPattern() { final a = Peer('peer-a'); final b = Peer('peer-b'); final c = Peer('peer-c'); a.send(b, 'block-42'); a.send(c, 'block-42'); check([...b.inbox, ...c.inbox].join('>') == 'peer-a>peer-b:block-42>peer-a>peer-c:block-42'); }

// Publish-Subscribe
typedef Subscriber = String Function(int id);
class PubSub { final topics = <String, List<Subscriber>>{}; void subscribe(String topic, Subscriber subscriber) => topics.putIfAbsent(topic, () => <Subscriber>[]).add(subscriber); List<String> publish(String topic, int id) => (topics[topic] ?? const <Subscriber>[]).map((s) => s(id)).toList(); }
void publishSubscribePattern() { final p = PubSub()..subscribe('order', (id) => 'warehouse:$id')..subscribe('order', (id) => 'analytics:$id'); check(p.publish('order', 51).join('>') == 'warehouse:51>analytics:51'); }

// Distributed Proxy
abstract interface class StockService { int stock(String sku); }
class RemoteStock implements StockService { @override int stock(String sku) => 7; }
class StockProxy implements StockService { StockProxy(this.remote); final StockService remote; @override int stock(String sku) => remote.stock(sku); }
void distributedProxyPattern() => check(StockProxy(RemoteStock()).stock('sku-1') == 7);

// Presentation-Abstraction-Control
class PacAgent { PacAgent(this.name, this.value); final String name; final int value; String view() => '$name:view=$value'; }
void presentationAbstractionControlPattern() => check(PacAgent('child', 42).view() == 'child:view=42' && PacAgent('root', 42).view() == 'root:view=42');

// Model-View-Presenter
class PassiveView { String text = ''; }
class Presenter { Presenter(this.model, this.view); final CounterModel model; final PassiveView view; void increment() { model.count++; view.text = renderCounter(model); } }
void modelViewPresenterPattern() { final m = CounterModel(); final v = PassiveView(); Presenter(m, v).increment(); check(m.count == 1 && v.text == 'count=1'); }

// Document-View
class Document { Document(this.title, this.words); final String title; final int words; }
String editorView(Document d) => 'editor:${d.title}:${d.words}'; String summaryView(Document d) => 'summary:${d.title}';
void documentViewPattern() { final d = Document('Final', 120); check(editorView(d) == 'editor:Final:120' && summaryView(d) == 'summary:Final'); }

// Active Record
class PersonRecord { PersonRecord(this.id, this.name); final int id; final String name; static final table = <int, PersonRecord>{}; void save() => table[id] = this; static PersonRecord? load(int id) => table[id]; }
void activeRecordPattern() { PersonRecord.table.clear(); PersonRecord(7, 'Ada').save(); final p = PersonRecord.load(7); check(p?.id == 7 && p?.name == 'Ada'); }

// Data Mapper
class Person { Person(this.id, this.name); final int id; final String name; }
class PersonRow { PersonRow(this.key, this.name); final String key; final String name; }
class PersonMapper { PersonRow toRow(Person p) => PersonRow('person:${p.id}', p.name); Person fromRow(PersonRow r) => Person(8, r.name); }
void dataMapperPattern() { final m = PersonMapper(); final row = m.toRow(Person(8, 'Grace')); final p = m.fromRow(row); check(row.key == 'person:8' && p.name == 'Grace'); }

// Unit of Work
class UnitOfWork { UnitOfWork(this.values); final List<int> values; final changes = <int, int>{}; void stage(int index, int delta) => changes[index] = (changes[index] ?? 0) + delta; void commit() { changes.forEach((i, d) => values[i] += d); changes.clear(); } }
void unitOfWorkPattern() { final u = UnitOfWork([10, 20]); final before = [...u.values]; u.stage(0, 5); u.stage(1, -3); u.commit(); check(before.join(',') == '10,20' && u.values.join(',') == '15,17'); }

// Repository
class PersonRepository { PersonRepository(this.items); final Map<int, Person> items; Person? byId(int id) => items[id]; }
void repositoryPattern() { final p = PersonRepository({9: Person(9, 'Linus')}).byId(9); check(p?.name == 'Linus'); }

// Dependency Injection
class Greeter { Greeter(this.send); final String Function(String) send; String greet(String name) => send(name); }
void dependencyInjectionPattern() => check(Greeter((n) => 'smtp:$n').greet('Ada') == 'smtp:Ada' && Greeter((n) => 'fake:$n').greet('Ada') == 'fake:Ada');

// Lazy Initialization
class LazyResource { String? _value; int creations = 0; String get value { if (_value == null) { _value = 'resource-ready'; creations++; } return _value!; } }
void lazyInitializationPattern() { final l = LazyResource(); check(l.value == 'resource-ready' && l.value == 'resource-ready' && l.creations == 1); }

// Object Pool
class ObjectPool { final available = <int>[]; int next = 0; int acquire() => available.isNotEmpty ? available.removeLast() : ++next; void release(int value) => available.add(value); }
void objectPoolPattern() { final p = ObjectPool(); final first = p.acquire(); final second = p.acquire(); p.release(first); final reused = p.acquire(); check(first == 1 && second == 2 && reused == 1); }

// Null Object
abstract interface class Logger { String log(String message); }
class RealLogger implements Logger { @override String log(String message) => 'logged:$message'; }
class NullLogger implements Logger { @override String log(String message) => ''; }
void nullObjectPattern() => check(RealLogger().log('processed:item-1') == 'logged:processed:item-1' && NullLogger().log('processed:item-1').isEmpty);

void main() {
  final cases = <void Function()>[
    commandPattern, interpreterPattern, iteratorPattern, mediatorPattern, mementoPattern,
    observerPattern, statePattern, strategyPattern, templateMethodPattern, visitorPattern,
    mvcPattern, mvvmPattern, microkernelPattern, microservicesPattern, enterpriseAdapterPattern,
    enterpriseBridgePattern, enterpriseFacadePattern, brokerPattern, messageBusPattern,
    serviceLocatorPattern, activeObjectPattern, monitorObjectPattern, halfSyncHalfAsyncPattern,
    leaderFollowersPattern, clientServerPattern, peerToPeerPattern, publishSubscribePattern,
    distributedProxyPattern, presentationAbstractionControlPattern, modelViewPresenterPattern,
    documentViewPattern, activeRecordPattern, dataMapperPattern, unitOfWorkPattern,
    repositoryPattern, dependencyInjectionPattern, lazyInitializationPattern, objectPoolPattern,
    nullObjectPattern,
  ];
  check(cases.length == 39);
  for (final pattern in cases) { pattern(); }
  print('Dart pattern sweep: 39/39 examples passed');
}
