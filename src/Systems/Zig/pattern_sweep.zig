const std = @import("std");

const Instruction = union(enum) {
    literal: i32,
    add,
    multiply,
};

fn commandExample() bool {
    const commands = [_]i32{ 10, -3 };
    var balance: i32 = 0;
    for (commands) |command| balance += command;
    const undone = balance - commands[commands.len - 1];
    return balance == 7 and undone == 10;
}

fn interpreterExample() bool {
    const program = [_]Instruction{
        .{ .literal = 2 },
        .{ .literal = 3 },
        .{ .literal = 4 },
        .multiply,
        .add,
    };
    var stack: [5]i32 = undefined;
    var top: usize = 0;
    for (program) |instruction| switch (instruction) {
        .literal => |value| {
            stack[top] = value;
            top += 1;
        },
        .add => {
            const right = stack[top - 1];
            const left = stack[top - 2];
            top -= 2;
            stack[top] = left + right;
            top += 1;
        },
        .multiply => {
            const right = stack[top - 1];
            const left = stack[top - 2];
            top -= 2;
            stack[top] = left * right;
            top += 1;
        },
    };
    return top == 1 and stack[0] == 14;
}

fn iteratorExample() bool {
    const values = [_]i32{ 10, 20 };
    var cursor: usize = 0;
    const first = values[cursor];
    cursor += 1;
    const second = values[cursor];
    cursor += 1;
    return first == 10 and second == 20 and cursor == values.len;
}

fn mediatorExample() bool {
    const sender = "sales";
    const recipient = if (std.mem.eql(u8, sender, "sales")) "billing" else "sales";
    return std.mem.eql(u8, recipient, "billing");
}

fn mementoExample() bool {
    var current: []const u8 = "v1";
    const snapshot = current;
    current = "v2";
    current = snapshot;
    return std.mem.eql(u8, current, "v1");
}

fn observerExample() bool {
    const Observer = *const fn (i32) i32;
    const audit = struct { fn call(value: i32) i32 { return value + 1; } }.call;
    const ui = struct { fn call(value: i32) i32 { return value * 2; } }.call;
    const observers = [_]Observer{ audit, ui };
    return observers[0](7) == 8 and observers[1](7) == 14;
}

fn stateExample() bool {
    const State = enum { logged_out, logged_in };
    var state: State = .logged_out;
    state = switch (state) { .logged_out => .logged_in, .logged_in => .logged_out };
    const first = state == .logged_in;
    state = switch (state) { .logged_out => .logged_in, .logged_in => .logged_out };
    return first and state == .logged_out;
}

fn strategyExample() bool {
    const Strategy = *const fn (i32) i32;
    const regular = struct { fn call(value: i32) i32 { return value; } }.call;
    const discounted = struct { fn call(value: i32) i32 { return @divTrunc(value * 80, 100); } }.call;
    const price = struct { fn call(strategy: Strategy, value: i32) i32 { return strategy(value); } }.call;
    return price(regular, 100) == 100 and price(discounted, 100) == 80;
}

fn templateMethodExample() bool {
    const Step = *const fn (i32) i32;
    const double = struct { fn call(value: i32) i32 { return value * 2; } }.call;
    const run = struct { fn call(step: Step, value: i32) i32 { return step(value) + 1; } }.call;
    return run(double, 3) == 7;
}

fn visitorExample() bool {
    const Shape = union(enum) { circle: i32, rectangle: struct { width: i32, height: i32 } };
    const area = struct {
        fn call(shape: Shape) i32 {
            return switch (shape) {
                .circle => |radius| 3 * radius * radius,
                .rectangle => |rectangle| rectangle.width * rectangle.height,
            };
        }
    }.call;
    return area(.{ .circle = 2 }) == 12 and area(.{ .rectangle = .{ .width = 3, .height = 4 } }) == 12;
}

fn mvcExample() bool {
    var model: i32 = 3;
    model += 1;
    return model == 4;
}

fn mvvmExample() bool {
    const ViewModel = struct { greeting: []const u8, enabled: bool };
    const vm = ViewModel{ .greeting = "Hello Ada", .enabled = true };
    return std.mem.eql(u8, vm.greeting, "Hello Ada") and vm.enabled;
}

fn microkernelExample() bool {
    const Plugin = *const fn (i32) i32;
    const double = struct { fn call(value: i32) i32 { return value * 2; } }.call;
    const square = struct { fn call(value: i32) i32 { return value * value; } }.call;
    const plugins = [_]Plugin{ double, square };
    return plugins[0](5) == 10 and plugins[1](3) == 9;
}

fn microservicesExample() bool {
    const inventory = struct { fn call(sku: []const u8) i32 { return if (std.mem.eql(u8, sku, "A")) 3 else 0; } }.call;
    const pricing = struct { fn call(sku: []const u8) i32 { return if (std.mem.eql(u8, sku, "A")) 20 else 0; } }.call;
    return inventory("A") == 3 and pricing("A") == 20;
}

fn enterpriseAdapterExample() bool {
    const legacy = struct { fn call(cents: i32) i32 { return cents; } }.call;
    const Adapter = struct {
        fn call(dollars: i32) i32 { return legacy(dollars * 100); }
    };
    return Adapter.call(12) == 1200;
}

fn enterpriseBridgeExample() bool {
    const Transport = *const fn ([]const u8) []const u8;
    const http = struct { fn call(_: []const u8) []const u8 { return "http:x"; } }.call;
    const queue = struct { fn call(_: []const u8) []const u8 { return "queue:x"; } }.call;
    const render = struct { fn call(transport: Transport) []const u8 { return transport("x"); } }.call;
    return std.mem.eql(u8, render(http), "http:x") and std.mem.eql(u8, render(queue), "queue:x");
}

fn enterpriseFacadeExample() bool {
    const validate = struct { fn call(value: i32) bool { return value > 0; } }.call;
    const facade = struct { fn call(value: i32) bool { return validate(value); } }.call;
    return facade(5) and !facade(0);
}

fn brokerExample() bool {
    const Service = *const fn (i32) i32;
    const tax = struct { fn call(value: i32) i32 { return @divTrunc(value * 16, 100); } }.call;
    const registry = [_]Service{tax};
    return registry[0](100) == 16;
}

fn messageBusExample() bool {
    const Handler = *const fn ([]const u8) usize;
    const audit = struct { fn call(message: []const u8) usize { return message.len + 1; } }.call;
    const mail = struct { fn call(message: []const u8) usize { return message.len + 2; } }.call;
    const handlers = [_]Handler{ audit, mail };
    return handlers[0]("paid") == 5 and handlers[1]("paid") == 6;
}

fn serviceLocatorExample() bool {
    const Service = struct { name: []const u8, value: []const u8 };
    const services = [_]Service{ .{ .name = "clock", .value = "12:00" }, .{ .name = "region", .value = "mx" } };
    return std.mem.eql(u8, services[1].name, "region") and std.mem.eql(u8, services[1].value, "mx");
}

fn activeObjectExample() bool {
    var queue = [_]?[]const u8{ "sync", null };
    const ran = queue[0].?;
    queue[0] = null;
    return std.mem.eql(u8, ran, "sync") and queue[0] == null;
}

fn monitorObjectExample() bool {
    var balance: i32 = 5;
    balance += 10;
    if (balance >= 7) balance -= 7;
    return balance == 8;
}

fn halfSyncHalfAsyncExample() bool {
    var queue = [_]?[]const u8{ "evt", null };
    const event = queue[0].?;
    queue[0] = null;
    return std.mem.eql(u8, event, "evt") and queue[0] == null;
}

fn leaderFollowersExample() bool {
    var pool = [_][]const u8{ "a", "b", "c" };
    const leader = pool[0];
    pool[0] = pool[1];
    pool[1] = pool[2];
    pool[2] = leader;
    return std.mem.eql(u8, leader, "a") and std.mem.eql(u8, pool[0], "b") and std.mem.eql(u8, pool[2], "a");
}

fn clientServerExample() bool {
    const server = struct { fn call(request: []const u8) bool { return std.mem.eql(u8, request, "ping"); } }.call;
    const client = struct { fn call(request: []const u8) bool { return server(request); } }.call;
    return client("ping");
}

fn peerToPeerExample() bool {
    const send = struct { fn call(from: []const u8, to: []const u8) bool { return from.len > 0 and to.len > 0; } }.call;
    return send("a", "b") and send("b", "a");
}

fn publishSubscribeExample() bool {
    const Subscription = struct { topic: []const u8, subscriber_count: usize };
    const orders = Subscription{ .topic = "orders", .subscriber_count = 2 };
    return std.mem.eql(u8, orders.topic, "orders") and orders.subscriber_count == 2;
}

fn distributedProxyExample() bool {
    const remote = struct { fn call(id: i32) i32 { return id * 10; } }.call;
    const proxy = struct { fn call(id: i32) i32 { return remote(id); } }.call;
    return proxy(7) == 70;
}

fn presentationAbstractionControlExample() bool {
    var abstraction: i32 = 4;
    abstraction += 1;
    return abstraction == 5;
}

fn modelViewPresenterExample() bool {
    const presenter = struct { fn call(name: []const u8) bool { return std.mem.eql(u8, name, "Ada"); } }.call;
    const passive_view = presenter("Ada");
    return passive_view;
}

fn documentViewExample() bool {
    const document = "hello";
    const plain = document;
    const upper = "HELLO";
    return std.mem.eql(u8, plain, "hello") and std.mem.eql(u8, upper, "HELLO");
}

fn activeRecordExample() bool {
    const ActiveRecord = struct {
        id: i32,
        name: []const u8,
        fn save(self: @This()) bool { return self.id == 1 and std.mem.eql(u8, self.name, "Ada"); }
    };
    return (ActiveRecord{ .id = 1, .name = "Ada" }).save();
}

fn dataMapperExample() bool {
    const Record = struct { id: i32, name: []const u8 };
    const Row = struct { id: i32, name: []const u8 };
    const mapper = struct {
        fn toRow(record: Record) Row { return .{ .id = record.id, .name = record.name }; }
        fn fromRow(row: Row) Record { return .{ .id = row.id, .name = row.name }; }
    };
    const row = mapper.toRow(.{ .id = 1, .name = "Ada" });
    const record = mapper.fromRow(row);
    return record.id == 1 and std.mem.eql(u8, record.name, "Ada");
}

fn unitOfWorkExample() bool {
    const Change = struct { id: i32, name: []const u8 };
    var pending = [_]?Change{ .{ .id = 1, .name = "Ada" }, null };
    var committed: ?Change = null;
    committed = pending[0];
    pending[0] = null;
    return committed.?.id == 1 and pending[0] == null;
}

fn repositoryExample() bool {
    const Entry = struct { id: i32, name: []const u8 };
    const repository = [_]Entry{.{ .id = 1, .name = "Ada" }};
    return repository[0].id == 1 and std.mem.eql(u8, repository[0].name, "Ada");
}

fn dependencyInjectionExample() bool {
    const Clock = *const fn () []const u8;
    const fixed = struct { fn call() []const u8 { return "12:00"; } }.call;
    const service = struct { fn call(clock: Clock) bool { return std.mem.eql(u8, clock(), "12:00"); } }.call;
    return service(fixed);
}

fn lazyInitializationExample() bool {
    var resource: ?[]const u8 = null;
    var created: usize = 0;
    if (resource == null) {
        resource = "resource";
        created += 1;
    }
    if (resource == null) {
        resource = "resource";
        created += 1;
    }
    return std.mem.eql(u8, resource.?, "resource") and created == 1;
}

fn objectPoolExample() bool {
    var pool = [_][]const u8{ "c1", "c2" };
    const acquired = pool[0];
    pool[0] = pool[1];
    pool[1] = acquired;
    return std.mem.eql(u8, pool[0], "c2") and std.mem.eql(u8, pool[1], "c1");
}

fn nullObjectExample() bool {
    const Logger = *const fn ([]const u8) usize;
    const real = struct { fn call(message: []const u8) usize { return message.len; } }.call;
    const null = struct { fn call(_: []const u8) usize { return 0; } }.call;
    const run = struct { fn call(logger: Logger, message: []const u8) usize { return logger(message); } }.call;
    return run(real, "x") == 1 and run(null, "x") == 0;
}

pub fn main() !void {
    const checks = [_]bool{
        commandExample(), interpreterExample(), iteratorExample(), mediatorExample(), mementoExample(),
        observerExample(), stateExample(), strategyExample(), templateMethodExample(), visitorExample(),
        mvcExample(), mvvmExample(), microkernelExample(), microservicesExample(), enterpriseAdapterExample(),
        enterpriseBridgeExample(), enterpriseFacadeExample(), brokerExample(), messageBusExample(), serviceLocatorExample(),
        activeObjectExample(), monitorObjectExample(), halfSyncHalfAsyncExample(), leaderFollowersExample(),
        clientServerExample(), peerToPeerExample(), publishSubscribeExample(), distributedProxyExample(),
        presentationAbstractionControlExample(), modelViewPresenterExample(), documentViewExample(), activeRecordExample(),
        dataMapperExample(), unitOfWorkExample(), repositoryExample(), dependencyInjectionExample(),
        lazyInitializationExample(), objectPoolExample(), nullObjectExample(),
    };

    var passed: usize = 0;
    for (checks) |ok| if (ok) { passed += 1; };
    if (passed != checks.len) return error.PatternSweepFailed;
    std.debug.print("Zig pattern sweep: {d}/{d} examples passed\n", .{ passed, checks.len });
}
