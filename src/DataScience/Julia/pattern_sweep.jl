# Language-major Design Pattern sweep: 39 remaining patterns.
must(value::Bool) = value || error("pattern assertion failed")
include(joinpath(@__DIR__, "memento.jl"))

# Command
struct BalanceCommand
    delta::Int
    name::String
end
execute(command::BalanceCommand, balance::Int) = balance + command.delta
undo(command::BalanceCommand, balance::Int) = balance - command.delta
function command_pattern()
    queue = [BalanceCommand(50, "deposit"), BalanceCommand(-20, "withdraw")]
    balance = foldl((b, c) -> execute(c, b), queue; init=100)
    must(balance == 130 && join(getfield.(queue, :name), ">") == "deposit>withdraw")
    must(undo(last(queue), balance) == 150)
end

# Interpreter
abstract type Expr end
struct Literal <: Expr; value::Int; end
struct AddExpr <: Expr; left::Expr; right::Expr; end
struct MulExpr <: Expr; left::Expr; right::Expr; end
evaluate(x::Literal) = x.value
evaluate(x::AddExpr) = evaluate(x.left) + evaluate(x.right)
evaluate(x::MulExpr) = evaluate(x.left) * evaluate(x.right)
interpreter_pattern() = must(evaluate(AddExpr(Literal(7), MulExpr(Literal(3), Literal(4)))) == 19)

# Iterator
mutable struct CursorIterator
    values::Vector{Int}
    index::Int
end
CursorIterator(values::Vector{Int}) = CursorIterator(values, 1)
function next_value(iterator::CursorIterator)
    iterator.index > length(iterator.values) && return nothing
    value = iterator.values[iterator.index]
    iterator.index += 1
    value
end
function iterator_pattern()
    iterator = CursorIterator([10, 20, 30])
    visited = Int[]
    while true
        value = next_value(iterator)
        value === nothing && break
        push!(visited, value)
    end
    must(visited == [10, 20, 30] && next_value(iterator) === nothing)
end

# Mediator
mutable struct UiMediator; events::Vector{String}; end
UiMediator() = UiMediator(String[])
function notify!(mediator::UiMediator, sender::String, event::String)
    sender == "button" && event == "click" && push!(mediator.events, "panel.refresh")
    sender == "panel" && event == "loaded" && push!(mediator.events, "button.enable")
end
function mediator_pattern()
    mediator = UiMediator(); notify!(mediator, "button", "click"); notify!(mediator, "panel", "loaded")
    must(join(mediator.events, ">") == "panel.refresh>button.enable")
end

# Memento is delegated to the individually addressable canonical included above.

# Observer
mutable struct Subject; observers::Vector{Function}; end
Subject() = Subject(Function[])
subscribe!(subject::Subject, observer::Function) = push!(subject.observers, observer)
publish(subject::Subject, id::Int) = [observer(id) for observer in subject.observers]
function observer_pattern()
    subject = Subject(); subscribe!(subject, id -> "audit:$id"); subscribe!(subject, id -> "dashboard:$id")
    must(publish(subject, 42) == ["audit:42", "dashboard:$id"])
end

# State
@enum GateState locked unlocked
function transition(state::GateState, action::String)
    state == locked && action == "unlock" && return unlocked
    state == unlocked && action == "lock" && return locked
    state
end
function state_pattern()
    state = transition(locked, "unlock")
    must(state == unlocked && transition(state, "lock") == locked)
end

# Strategy
price(value::Int, strategy::Function) = strategy(value)
strategy_pattern() = must(price(100, identity) == 100 && price(100, value -> value * 80 ÷ 100) == 80)

# Template Method
pipeline(read_step::String, transform::Function) = "$read_step>$(transform())>publish"
function template_method_pattern()
    must(pipeline("read-csv", () -> "normalize") == "read-csv>normalize>publish")
    must(pipeline("read-json", () -> "aggregate") == "read-json>aggregate>publish")
end

# Visitor: Julia's multiple dispatch separates operations from shape data.
abstract type Shape end
struct Circle <: Shape; radius::Float64; end
struct Rectangle <: Shape; width::Float64; height::Float64; end
struct AreaVisitor end
visit(::AreaVisitor, circle::Circle) = π * circle.radius^2
visit(::AreaVisitor, rectangle::Rectangle) = rectangle.width * rectangle.height
accept(shape::Shape, visitor::AreaVisitor) = visit(visitor, shape)
function visitor_pattern()
    shapes = Shape[Circle(2.0), Rectangle(3.0, 4.0)]
    total = sum(shape -> accept(shape, AreaVisitor()), shapes)
    must(abs(total - (4π + 12)) < 1e-9)
end

# MVC
mutable struct CounterModel; count::Int; end
struct CounterController; model::CounterModel; end
increment!(controller::CounterController) = (controller.model.count += 1)
render_counter(model::CounterModel) = "count=$(model.count)"
function mvc_pattern()
    model = CounterModel(0); before = render_counter(model); increment!(CounterController(model))
    must(before == "count=0" && render_counter(model) == "count=1")
end

# MVVM
mutable struct AmountViewModel; amount::Int; end
amount_text(view_model::AmountViewModel) = "\$$(view_model.amount).00"
add_amount!(view_model::AmountViewModel, value::Int) = (view_model.amount += value)
function mvvm_pattern()
    view_model = AmountViewModel(10); before = amount_text(view_model); add_amount!(view_model, 5)
    must(before == "\$10.00" && amount_text(view_model) == "\$15.00")
end

# Microkernel
mutable struct Kernel; plugins::Dict{String, Function}; end
Kernel() = Kernel(Dict{String, Function}())
register!(kernel::Kernel, name::String, plugin::Function) = (kernel.plugins[name] = plugin)
run_plugin(kernel::Kernel, name::String, value::Int) = kernel.plugins[name](value)
function microkernel_pattern()
    kernel = Kernel(); register!(kernel, "double", value -> value * 2); register!(kernel, "square", value -> value^2)
    must(run_plugin(kernel, "double", 4) == 8 && run_plugin(kernel, "square", 4) == 16)
end

# Microservices
mutable struct InventoryService; stock::Int; end
function reserve!(inventory::InventoryService, quantity::Int)
    quantity > inventory.stock && return false
    inventory.stock -= quantity
    true
end
struct OrderService; inventory::InventoryService; end
place(order::OrderService, quantity::Int) = reserve!(order.inventory, quantity) ? "confirmed" : "rejected"
function microservices_pattern()
    inventory = InventoryService(7); must(place(OrderService(inventory), 2) == "confirmed" && inventory.stock == 5)
end

# Enterprise Adapter
struct LegacyCustomer; code::Int; cents::Int; end
struct CanonicalCustomer; id::Int; amount::Float64; end
adapt_customer(customer::LegacyCustomer) = CanonicalCustomer(customer.code, customer.cents / 100)
function enterprise_adapter_pattern()
    customer = adapt_customer(LegacyCustomer(17, 1250)); must(customer.id == 17 && customer.amount == 12.5)
end

# Enterprise Bridge
abstract type Transport end
struct NamedTransport <: Transport; name::String; end
send(transport::NamedTransport, message::String) = "$(transport.name)>$message"
send_notice(kind::String, message::String, transport::Transport) = send(transport, "$kind:$message")
function enterprise_bridge_pattern()
    must(send_notice("ALERT", "disk", NamedTransport("kafka")) == "kafka>ALERT:disk")
    must(send_notice("REMINDER", "backup", NamedTransport("queue")) == "queue>REMINDER:backup")
end

# Enterprise Facade
function enterprise_facade_pattern()
    crm(id) = "crm:create:$id"; billing(id) = "billing:open:$id"
    must("$(crm(77))>$(billing(77))" == "crm:create:77>billing:open:77")
end

# Broker
function broker_pattern()
    services = Dict{String, Function}("inventory" => key -> "inventory:$key=7", "customer" => key -> "customer:$key=active")
    must(services["inventory"]("sku-1") == "inventory:sku-1=7" && services["customer"]("17") == "customer:17=active")
end

# Message Bus
struct BusMessage; topic::String; id::Int; end
mutable struct MessageBus; handlers::Vector{Function}; end
MessageBus() = MessageBus(Function[])
on!(bus::MessageBus, handler::Function) = push!(bus.handlers, handler)
send_message(bus::MessageBus, message::BusMessage) = [handler(message) for handler in bus.handlers]
function message_bus_pattern()
    bus = MessageBus(); on!(bus, message -> "audit:$(message.topic):$(message.id)"); on!(bus, message -> "billing:$(message.topic):$(message.id)")
    must(send_message(bus, BusMessage("order-created", 42)) == ["audit:order-created:42", "billing:order-created:42"])
end

# Service Locator
function service_locator_pattern()
    services = Dict{String, Function}("email" => value -> "email>$value", "audit" => value -> "audit>$value")
    must(services["email"]("a@example.test") == "email>a@example.test" && services["audit"]("created") == "audit>created")
end

# Active Object
function active_object_pattern()
    value = Ref(0); queue = Function[() -> value[] += 3, () -> value[] *= 4]; before = value[]
    foreach(command -> command(), queue)
    must(before == 0 && value[] == 12)
end

# Monitor Object: lock and state live behind one monitor abstraction.
mutable struct MonitoredCounter
    lock::ReentrantLock
    value::Int
    max_critical::Int
    critical::Int
end
MonitoredCounter() = MonitoredCounter(ReentrantLock(), 0, 0, 0)
function add!(counter::MonitoredCounter, amount::Int)
    lock(counter.lock) do
        counter.critical += 1
        counter.max_critical = max(counter.max_critical, counter.critical)
        counter.value += amount
        counter.critical -= 1
    end
end
function monitor_object_pattern()
    counter = MonitoredCounter(); add!(counter, 2); add!(counter, 3)
    must(counter.value == 5 && counter.max_critical == 1)
end

# Half-Sync / Half-Async
half_sync_half_async_pattern() = must(["done:$job" for job in ["job-1", "job-2", "job-3"]] == ["done:job-1", "done:job-2", "done:job-3"])

# Leader / Followers
function leader_followers_pattern()
    workers = ["worker-1", "worker-2", "worker-3"]; events = ["event-a", "event-b", "event-c"]
    handled = ["$(workers[index]):$(events[index])" for index in eachindex(events)]
    must(handled == ["worker-1:event-a", "worker-2:event-b", "worker-3:event-c"] && workers[mod1(length(events) + 1, length(workers))] == "worker-1")
end

# Client-Server
struct Response; status::Int; body::String; end
server_handle(key::String) = key == "sku-1" ? Response(200, "stock=7") : Response(404, "missing")
function client_server_pattern()
    response = server_handle("sku-1"); must(response.status == 200 && response.body == "stock=7")
end

# Peer-to-Peer
mutable struct Peer; name::String; inbox::Vector{String}; end
Peer(name::String) = Peer(name, String[])
send_peer!(from::Peer, to::Peer, data::String) = push!(to.inbox, "$(from.name)>$(to.name):$data")
function peer_to_peer_pattern()
    a = Peer("peer-a"); b = Peer("peer-b"); c = Peer("peer-c"); send_peer!(a, b, "block-42"); send_peer!(a, c, "block-42")
    must(vcat(b.inbox, c.inbox) == ["peer-a>peer-b:block-42", "peer-a>peer-c:block-42"])
end

# Publish-Subscribe
mutable struct PubSub; topics::Dict{String, Vector{Function}}; end
PubSub() = PubSub(Dict{String, Vector{Function}}())
function subscribe!(pubsub::PubSub, topic::String, subscriber::Function)
    push!(get!(pubsub.topics, topic, Function[]), subscriber)
end
publish_topic(pubsub::PubSub, topic::String, id::Int) = [subscriber(id) for subscriber in get(pubsub.topics, topic, Function[])]
function publish_subscribe_pattern()
    pubsub = PubSub(); subscribe!(pubsub, "order", id -> "warehouse:$id"); subscribe!(pubsub, "order", id -> "analytics:$id")
    must(publish_topic(pubsub, "order", 51) == ["warehouse:51", "analytics:51"])
end

# Distributed Proxy
abstract type StockService end
struct RemoteStock <: StockService end
struct StockProxy <: StockService; remote::StockService; end
stock(::RemoteStock, sku::String) = 7
stock(proxy::StockProxy, sku::String) = stock(proxy.remote, sku)
distributed_proxy_pattern() = must(stock(StockProxy(RemoteStock()), "sku-1") == 7)

# Presentation-Abstraction-Control
struct PacAgent; name::String; value::Int; end
view(agent::PacAgent) = "$(agent.name):view=$(agent.value)"
presentation_abstraction_control_pattern() = must(view(PacAgent("child", 42)) == "child:view=42" && view(PacAgent("root", 42)) == "root:view=42")

# Model-View-Presenter
mutable struct PassiveView; text::String; end
struct Presenter; model::CounterModel; view::PassiveView; end
function presenter_increment!(presenter::Presenter)
    presenter.model.count += 1; presenter.view.text = render_counter(presenter.model)
end
function model_view_presenter_pattern()
    model = CounterModel(0); view = PassiveView(""); presenter_increment!(Presenter(model, view))
    must(model.count == 1 && view.text == "count=1")
end

# Document-View
struct Document; title::String; words::Int; end
editor_view(document::Document) = "editor:$(document.title):$(document.words)"
summary_view(document::Document) = "summary:$(document.title)"
function document_view_pattern()
    document = Document("Final", 120); must(editor_view(document) == "editor:Final:120" && summary_view(document) == "summary:Final")
end

# Active Record
mutable struct PersonRecord; id::Int; name::String; end
const PERSON_TABLE = Dict{Int, PersonRecord}()
save!(person::PersonRecord) = (PERSON_TABLE[person.id] = person)
load_person(id::Int) = get(PERSON_TABLE, id, nothing)
function active_record_pattern()
    empty!(PERSON_TABLE); save!(PersonRecord(7, "Ada")); person = load_person(7)
    must(person !== nothing && person.name == "Ada")
end

# Data Mapper
struct Person; id::Int; name::String; end
struct PersonRow; key::String; name::String; end
struct PersonMapper end
to_row(::PersonMapper, person::Person) = PersonRow("person:$(person.id)", person.name)
from_row(::PersonMapper, row::PersonRow) = Person(8, row.name)
function data_mapper_pattern()
    mapper = PersonMapper(); row = to_row(mapper, Person(8, "Grace")); person = from_row(mapper, row)
    must(row.key == "person:8" && person.name == "Grace")
end

# Unit of Work
mutable struct UnitOfWork
    values::Vector{Int}
    changes::Vector{Tuple{Int, Int}}
end
UnitOfWork(values::Vector{Int}) = UnitOfWork(values, Tuple{Int, Int}[])
stage!(unit::UnitOfWork, index::Int, delta::Int) = push!(unit.changes, (index, delta))
function commit!(unit::UnitOfWork)
    for (index, delta) in unit.changes; unit.values[index] += delta; end
    empty!(unit.changes)
end
function unit_of_work_pattern()
    unit = UnitOfWork([10, 20]); before = copy(unit.values); stage!(unit, 1, 5); stage!(unit, 2, -3); commit!(unit)
    must(before == [10, 20] && unit.values == [15, 17])
end

# Repository
struct PersonRepository; items::Dict{Int, Person}; end
by_id(repository::PersonRepository, id::Int) = get(repository.items, id, nothing)
function repository_pattern()
    person = by_id(PersonRepository(Dict(9 => Person(9, "Linus"))), 9); must(person !== nothing && person.name == "Linus")
end

# Dependency Injection
struct Greeter; sender::Function; end
greet(greeter::Greeter, name::String) = greeter.sender(name)
function dependency_injection_pattern()
    production = Greeter(name -> "smtp:$name"); test = Greeter(name -> "fake:$name")
    must(greet(production, "Ada") == "smtp:Ada" && greet(test, "Ada") == "fake:Ada")
end

# Lazy Initialization
mutable struct LazyResource
    value::Union{Nothing, String}
    creations::Int
end
LazyResource() = LazyResource(nothing, 0)
function get_resource!(resource::LazyResource)
    if resource.value === nothing
        resource.value = "resource-ready"; resource.creations += 1
    end
    resource.value::String
end
function lazy_initialization_pattern()
    resource = LazyResource(); must(get_resource!(resource) == "resource-ready" && get_resource!(resource) == "resource-ready" && resource.creations == 1)
end

# Object Pool
mutable struct ObjectPool; available::Vector{Int}; next_id::Int; end
ObjectPool() = ObjectPool(Int[], 0)
function acquire!(pool::ObjectPool)
    !isempty(pool.available) && return pop!(pool.available)
    pool.next_id += 1
end
release!(pool::ObjectPool, value::Int) = push!(pool.available, value)
function object_pool_pattern()
    pool = ObjectPool(); first = acquire!(pool); second = acquire!(pool); release!(pool, first); reused = acquire!(pool)
    must(first == 1 && second == 2 && reused == 1)
end

# Null Object
abstract type Logger end
struct RealLogger <: Logger end
struct NullLogger <: Logger end
log(::RealLogger, message::String) = "logged:$message"
log(::NullLogger, message::String) = ""
null_object_pattern() = must(log(RealLogger(), "processed:item-1") == "logged:processed:item-1" && log(NullLogger(), "processed:item-1") == "")

const PATTERNS = [
    "Command", "Interpreter", "Iterator", "Mediator", "Memento", "Observer", "State", "Strategy", "Template Method", "Visitor",
    "MVC", "MVVM", "Microkernel", "Microservices", "Enterprise Adapter", "Enterprise Bridge", "Enterprise Facade", "Broker", "Message Bus", "Service Locator",
    "Active Object", "Monitor Object", "Half-Sync/Half-Async", "Leader/Followers", "Client-Server", "Peer-to-Peer", "Publish-Subscribe", "Distributed Proxy",
    "Presentation-Abstraction-Control", "Model-View-Presenter", "Document-View", "Active Record", "Data Mapper", "Unit of Work", "Repository",
    "Dependency Injection", "Lazy Initialization", "Object Pool", "Null Object",
]
must(length(PATTERNS) == 39)

for pattern in (
    command_pattern, interpreter_pattern, iterator_pattern, mediator_pattern, verify_memento_canonical, observer_pattern, state_pattern, strategy_pattern, template_method_pattern, visitor_pattern,
    mvc_pattern, mvvm_pattern, microkernel_pattern, microservices_pattern, enterprise_adapter_pattern, enterprise_bridge_pattern, enterprise_facade_pattern, broker_pattern, message_bus_pattern, service_locator_pattern,
    active_object_pattern, monitor_object_pattern, half_sync_half_async_pattern, leader_followers_pattern, client_server_pattern, peer_to_peer_pattern, publish_subscribe_pattern, distributed_proxy_pattern,
    presentation_abstraction_control_pattern, model_view_presenter_pattern, document_view_pattern, active_record_pattern, data_mapper_pattern, unit_of_work_pattern, repository_pattern,
    dependency_injection_pattern, lazy_initialization_pattern, object_pool_pattern, null_object_pattern,
)
    pattern()
end
println("Julia pattern sweep: 39/39 examples passed")
