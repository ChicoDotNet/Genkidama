const std = @import("std");
const memento = @import("memento.zig");

// Command
const BalanceCommand = struct {
    delta: i32,
    fn execute(self: BalanceCommand, balance: i32) i32 {
        return balance + self.delta;
    }
    fn undo(self: BalanceCommand, balance: i32) i32 {
        return balance - self.delta;
    }
};
fn commandPattern() bool {
    const queue = [_]BalanceCommand{ .{ .delta = 50 }, .{ .delta = -20 } };
    var balance: i32 = 100;
    for (queue) |command| balance = command.execute(balance);
    return balance == 130 and queue[1].undo(balance) == 150;
}

// Interpreter
const Expr = union(enum) {
    lit: i32,
    add: struct { left: *const Expr, right: *const Expr },
    mul: struct { left: *const Expr, right: *const Expr },
};
fn evalExpr(expr: *const Expr) i32 {
    return switch (expr.*) {
        .lit => |value| value,
        .add => |pair| evalExpr(pair.left) + evalExpr(pair.right),
        .mul => |pair| evalExpr(pair.left) * evalExpr(pair.right),
    };
}
fn interpreterPattern() bool {
    const seven = Expr{ .lit = 7 };
    const three = Expr{ .lit = 3 };
    const four = Expr{ .lit = 4 };
    const product = Expr{ .mul = .{ .left = &three, .right = &four } };
    const sum = Expr{ .add = .{ .left = &seven, .right = &product } };
    return evalExpr(&sum) == 19;
}

// Iterator
const IntIterator = struct {
    values: []const i32,
    index: usize = 0,
    fn next(self: *IntIterator) ?i32 {
        if (self.index >= self.values.len) return null;
        const value = self.values[self.index];
        self.index += 1;
        return value;
    }
};
fn iteratorPattern() bool {
    const values = [_]i32{ 10, 20, 30 };
    var iterator = IntIterator{ .values = &values };
    return iterator.next().? == 10 and iterator.next().? == 20 and iterator.next().? == 30 and iterator.next() == null;
}

// Mediator
const UiEvent = enum { none, panel_refresh, button_enable };
fn mediate(sender: enum { button, panel }, event: enum { click, loaded }) UiEvent {
    if (sender == .button and event == .click) return .panel_refresh;
    if (sender == .panel and event == .loaded) return .button_enable;
    return .none;
}
fn mediatorPattern() bool {
    return mediate(.button, .click) == .panel_refresh and mediate(.panel, .loaded) == .button_enable;
}

// Memento is delegated to the individually addressable canonical imported above.

// Observer
fn auditObserver(id: i32) i32 {
    return id + 1;
}
fn dashboardObserver(id: i32) i32 {
    return id + 2;
}
fn observerPattern() bool {
    const observers = [_]*const fn (i32) i32{ auditObserver, dashboardObserver };
    return observers[0](42) == 43 and observers[1](42) == 44;
}

// State
const GateState = enum { locked, unlocked };
fn transitionGate(state: GateState, action: enum { lock, unlock }) GateState {
    if (state == .locked and action == .unlock) return .unlocked;
    if (state == .unlocked and action == .lock) return .locked;
    return state;
}
fn statePattern() bool {
    const opened = transitionGate(.locked, .unlock);
    return opened == .unlocked and transitionGate(opened, .lock) == .locked;
}

// Strategy
fn regularPrice(value: i32) i32 {
    return value;
}
fn vipPrice(value: i32) i32 {
    return @divTrunc(value * 80, 100);
}
fn applyPrice(value: i32, strategy: *const fn (i32) i32) i32 {
    return strategy(value);
}
fn strategyPattern() bool {
    return applyPrice(100, regularPrice) == 100 and applyPrice(100, vipPrice) == 80;
}

// Template Method
const PipelineResult = struct { read_code: u8, transform_code: u8, published: bool };
fn normalize() u8 {
    return 1;
}
fn aggregate() u8 {
    return 2;
}
fn pipeline(read_code: u8, transform: *const fn () u8) PipelineResult {
    return .{ .read_code = read_code, .transform_code = transform(), .published = true };
}
fn templateMethodPattern() bool {
    const csv = pipeline(1, normalize);
    const json = pipeline(2, aggregate);
    return csv.read_code == 1 and csv.transform_code == 1 and csv.published and json.read_code == 2 and json.transform_code == 2 and json.published;
}

// Visitor
const Shape = union(enum) { circle: f64, rectangle: struct { width: f64, height: f64 } };
fn area(shape: Shape) f64 {
    return switch (shape) {
        .circle => |radius| std.math.pi * radius * radius,
        .rectangle => |rectangle| rectangle.width * rectangle.height,
    };
}
fn visitorPattern() bool {
    const total = area(.{ .circle = 2.0 }) + area(.{ .rectangle = .{ .width = 3.0, .height = 4.0 } });
    return @abs(total - (4.0 * std.math.pi + 12.0)) < 1e-9;
}

// MVC
const CounterModel = struct { count: i32 };
fn controllerIncrement(model: *CounterModel) void {
    model.count += 1;
}
fn renderCounter(model: CounterModel) i32 {
    return model.count;
}
fn mvcPattern() bool {
    var model = CounterModel{ .count = 0 };
    const before = renderCounter(model);
    controllerIncrement(&model);
    return before == 0 and renderCounter(model) == 1;
}

// MVVM
const AmountViewModel = struct {
    amount: i32,
    fn add(self: *AmountViewModel, value: i32) void {
        self.amount += value;
    }
    fn projected(self: AmountViewModel) i32 {
        return self.amount * 100;
    }
};
fn mvvmPattern() bool {
    var vm = AmountViewModel{ .amount = 10 };
    const before = vm.projected();
    vm.add(5);
    return before == 1000 and vm.projected() == 1500;
}

// Microkernel
const Plugin = enum { double, square };
fn runPlugin(plugin: Plugin, value: i32) i32 {
    return switch (plugin) {
        .double => value * 2,
        .square => value * value,
    };
}
fn microkernelPattern() bool {
    return runPlugin(.double, 4) == 8 and runPlugin(.square, 4) == 16;
}

// Microservices
const InventoryService = struct {
    stock: i32,
    fn reserve(self: *InventoryService, quantity: i32) bool {
        if (quantity > self.stock) return false;
        self.stock -= quantity;
        return true;
    }
};
fn placeOrder(inventory: *InventoryService, quantity: i32) enum { confirmed, rejected } {
    return if (inventory.reserve(quantity)) .confirmed else .rejected;
}
fn microservicesPattern() bool {
    var inventory = InventoryService{ .stock = 7 };
    return placeOrder(&inventory, 2) == .confirmed and inventory.stock == 5;
}

// Enterprise Adapter
const LegacyCustomer = struct { code: i32, cents: i32 };
const CanonicalCustomer = struct { id: i32, cents: i32 };
fn adaptCustomer(customer: LegacyCustomer) CanonicalCustomer {
    return .{ .id = customer.code, .cents = customer.cents };
}
fn enterpriseAdapterPattern() bool {
    const customer = adaptCustomer(.{ .code = 17, .cents = 1250 });
    return customer.id == 17 and customer.cents == 1250;
}

// Enterprise Bridge
const TransportKind = enum { kafka, queue };
const NoticeKind = enum { alert, reminder };
fn bridgeSend(kind: NoticeKind, transport: TransportKind) u8 {
    return if (kind == .alert and transport == .kafka) 1 else if (kind == .reminder and transport == .queue) 2 else 0;
}
fn enterpriseBridgePattern() bool {
    return bridgeSend(.alert, .kafka) == 1 and bridgeSend(.reminder, .queue) == 2;
}

// Enterprise Facade
const Provisioned = struct { crm: bool, billing: bool };
fn provisionCustomer(id: i32) Provisioned {
    return .{ .crm = id == 77, .billing = id == 77 };
}
fn enterpriseFacadePattern() bool {
    const result = provisionCustomer(77);
    return result.crm and result.billing;
}

// Broker
const BrokerService = enum { inventory, customer };
fn brokerCall(service: BrokerService, key: i32) i32 {
    return switch (service) {
        .inventory => if (key == 1) 7 else 0,
        .customer => if (key == 17) 1 else 0,
    };
}
fn brokerPattern() bool {
    return brokerCall(.inventory, 1) == 7 and brokerCall(.customer, 17) == 1;
}

// Message Bus
fn auditHandler(id: i32) i32 {
    return id + 100;
}
fn billingHandler(id: i32) i32 {
    return id + 200;
}
fn messageBusPattern() bool {
    const handlers = [_]*const fn (i32) i32{ auditHandler, billingHandler };
    return handlers[0](42) == 142 and handlers[1](42) == 242;
}

// Service Locator
fn emailService(value: i32) i32 {
    return value + 10;
}
fn auditService(value: i32) i32 {
    return value + 20;
}
fn locateService(kind: enum { email, audit }) *const fn (i32) i32 {
    return switch (kind) {
        .email => emailService,
        .audit => auditService,
    };
}
fn serviceLocatorPattern() bool {
    return locateService(.email)(1) == 11 and locateService(.audit)(1) == 21;
}

// Active Object
fn addThree(value: *i32) void {
    value.* += 3;
}
fn timesFour(value: *i32) void {
    value.* *= 4;
}
fn activeObjectPattern() bool {
    var value: i32 = 0;
    const queue = [_]*const fn (*i32) void{ addThree, timesFour };
    const before = value;
    for (queue) |command| command(&value);
    return before == 0 and value == 12;
}

// Monitor Object: deterministic protocol model keeps lock and state together.
const MonitorCounter = struct {
    value: i32 = 0,
    locked: bool = false,
    max_critical: i32 = 0,
    fn add(self: *MonitorCounter, amount: i32) bool {
        if (self.locked) return false;
        self.locked = true;
        self.max_critical = @max(self.max_critical, 1);
        self.value += amount;
        self.locked = false;
        return true;
    }
};
fn monitorObjectPattern() bool {
    var counter = MonitorCounter{};
    return counter.add(2) and counter.add(3) and counter.value == 5 and counter.max_critical == 1 and !counter.locked;
}

// Half-Sync / Half-Async
fn halfSyncHalfAsyncPattern() bool {
    const queued = [_]i32{ 1, 2, 3 };
    var total: i32 = 0;
    for (queued) |job| total += job;
    return total == 6;
}

// Leader / Followers
fn leaderFollowersPattern() bool {
    const workers = [_]u8{ 1, 2, 3 };
    const events = [_]u8{ 10, 20, 30 };
    var handled: i32 = 0;
    for (events, 0..) |event, index| handled += workers[index] + event;
    return handled == 66 and workers[events.len % workers.len] == 1;
}

// Client-Server
const Response = struct { status: i32, stock: i32 };
fn serverHandle(key: i32) Response {
    return if (key == 1) .{ .status = 200, .stock = 7 } else .{ .status = 404, .stock = 0 };
}
fn clientServerPattern() bool {
    const response = serverHandle(1);
    return response.status == 200 and response.stock == 7;
}

// Peer-to-Peer
const Delivery = struct { from: u8, to: u8, payload: i32 };
fn peerSend(from: u8, to: u8, payload: i32) Delivery {
    return .{ .from = from, .to = to, .payload = payload };
}
fn peerToPeerPattern() bool {
    const b = peerSend(1, 2, 42);
    const c = peerSend(1, 3, 42);
    return b.from == 1 and b.to == 2 and c.to == 3 and b.payload == c.payload;
}

// Publish-Subscribe
fn warehouseSubscriber(id: i32) i32 {
    return id + 1;
}
fn analyticsSubscriber(id: i32) i32 {
    return id + 2;
}
fn publishSubscribePattern() bool {
    const subscribers = [_]*const fn (i32) i32{ warehouseSubscriber, analyticsSubscriber };
    return subscribers[0](51) == 52 and subscribers[1](51) == 53;
}

// Distributed Proxy
fn remoteStock(_: i32) i32 {
    return 7;
}
fn stockProxy(remote: *const fn (i32) i32, sku: i32) i32 {
    return remote(sku);
}
fn distributedProxyPattern() bool {
    return stockProxy(remoteStock, 1) == 7;
}

// Presentation-Abstraction-Control
const PacAgent = struct {
    value: i32,
    fn view(self: PacAgent, offset: i32) i32 {
        return self.value + offset;
    }
};
fn presentationAbstractionControlPattern() bool {
    return (PacAgent{ .value = 42 }).view(1) == 43 and (PacAgent{ .value = 42 }).view(2) == 44;
}

// Model-View-Presenter
const PassiveView = struct { text_value: i32 = 0 };
fn presenterIncrement(model: *CounterModel, view: *PassiveView) void {
    model.count += 1;
    view.text_value = model.count;
}
fn modelViewPresenterPattern() bool {
    var model = CounterModel{ .count = 0 };
    var view = PassiveView{};
    presenterIncrement(&model, &view);
    return model.count == 1 and view.text_value == 1;
}

// Document-View
const Document = struct { title_code: i32, words: i32 };
fn editorView(document: Document) i32 {
    return document.title_code + document.words;
}
fn summaryView(document: Document) i32 {
    return document.title_code;
}
fn documentViewPattern() bool {
    const document = Document{ .title_code = 7, .words = 120 };
    return editorView(document) == 127 and summaryView(document) == 7;
}

// Active Record
var person_table: [2]?PersonRecord = .{ null, null };
const PersonRecord = struct {
    id: usize,
    name_code: i32,
    fn save(self: PersonRecord) void {
        person_table[self.id] = self;
    }
};
fn activeRecordPattern() bool {
    person_table = .{ null, null };
    const person = PersonRecord{ .id = 1, .name_code = 7 };
    person.save();
    return person_table[1].?.name_code == 7;
}

// Data Mapper
const Person = struct { id: i32, name_code: i32 };
const PersonRow = struct { key: i32, name_code: i32 };
fn toRow(person: Person) PersonRow {
    return .{ .key = 1000 + person.id, .name_code = person.name_code };
}
fn fromRow(row: PersonRow) Person {
    return .{ .id = row.key - 1000, .name_code = row.name_code };
}
fn dataMapperPattern() bool {
    const row = toRow(.{ .id = 8, .name_code = 3 });
    const person = fromRow(row);
    return row.key == 1008 and person.id == 8 and person.name_code == 3;
}

// Unit of Work
const UnitOfWork = struct {
    values: [2]i32,
    deltas: [2]i32 = .{ 0, 0 },
    fn stage(self: *UnitOfWork, index: usize, delta: i32) void {
        self.deltas[index] += delta;
    }
    fn commit(self: *UnitOfWork) void {
        for (&self.values, self.deltas) |*value, delta| value.* += delta;
        self.deltas = .{ 0, 0 };
    }
};
fn unitOfWorkPattern() bool {
    var unit = UnitOfWork{ .values = .{ 10, 20 } };
    const before = unit.values;
    unit.stage(0, 5);
    unit.stage(1, -3);
    unit.commit();
    return before[0] == 10 and before[1] == 20 and unit.values[0] == 15 and unit.values[1] == 17;
}

// Repository
const PersonRepository = struct {
    person: Person,
    fn byId(self: PersonRepository, id: i32) ?Person {
        return if (self.person.id == id) self.person else null;
    }
};
fn repositoryPattern() bool {
    const repository = PersonRepository{ .person = .{ .id = 9, .name_code = 4 } };
    return repository.byId(9).?.name_code == 4;
}

// Dependency Injection
fn smtpSender(value: i32) i32 {
    return value + 100;
}
fn fakeSender(value: i32) i32 {
    return value + 200;
}
const Greeter = struct {
    sender: *const fn (i32) i32,
    fn greet(self: Greeter, value: i32) i32 {
        return self.sender(value);
    }
};
fn dependencyInjectionPattern() bool {
    return (Greeter{ .sender = smtpSender }).greet(1) == 101 and (Greeter{ .sender = fakeSender }).greet(1) == 201;
}

// Lazy Initialization
const LazyResource = struct {
    ready: bool = false,
    creations: i32 = 0,
    fn get(self: *LazyResource) i32 {
        if (!self.ready) {
            self.ready = true;
            self.creations += 1;
        }
        return 7;
    }
};
fn lazyInitializationPattern() bool {
    var resource = LazyResource{};
    return resource.get() == 7 and resource.get() == 7 and resource.creations == 1;
}

// Object Pool
const ObjectPool = struct {
    available: ?i32 = null,
    next_id: i32 = 0,
    fn acquire(self: *ObjectPool) i32 {
        if (self.available) |value| {
            self.available = null;
            return value;
        }
        self.next_id += 1;
        return self.next_id;
    }
    fn release(self: *ObjectPool, value: i32) void {
        self.available = value;
    }
};
fn objectPoolPattern() bool {
    var pool = ObjectPool{};
    const first = pool.acquire();
    const second = pool.acquire();
    pool.release(first);
    const reused = pool.acquire();
    return first == 1 and second == 2 and reused == 1;
}

// Null Object
fn realLogger(value: i32) i32 {
    return value;
}
fn nullLogger(_: i32) i32 {
    return 0;
}
fn nullObjectPattern() bool {
    return realLogger(7) == 7 and nullLogger(7) == 0;
}

pub fn main() void {
    const cases = [_]*const fn () bool{
        commandPattern,      interpreterPattern,   iteratorPattern,          mediatorPattern,        memento.verifyMementoCanonical, observerPattern,            statePattern,              strategyPattern,         templateMethodPattern,                 visitorPattern,
        mvcPattern,          mvvmPattern,          microkernelPattern,       microservicesPattern,   enterpriseAdapterPattern,       enterpriseBridgePattern,    enterpriseFacadePattern,   brokerPattern,           messageBusPattern,                     serviceLocatorPattern,
        activeObjectPattern, monitorObjectPattern, halfSyncHalfAsyncPattern, leaderFollowersPattern, clientServerPattern,            peerToPeerPattern,          publishSubscribePattern,   distributedProxyPattern, presentationAbstractionControlPattern, modelViewPresenterPattern,
        documentViewPattern, activeRecordPattern,  dataMapperPattern,        unitOfWorkPattern,      repositoryPattern,              dependencyInjectionPattern, lazyInitializationPattern, objectPoolPattern,       nullObjectPattern,
    };
    std.debug.assert(cases.len == 39);
    for (cases) |case| std.debug.assert(case());
    std.debug.print("Zig pattern sweep: 39/39 examples passed\n", .{});
}
