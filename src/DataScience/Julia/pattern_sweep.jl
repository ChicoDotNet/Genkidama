evalexpr(x::Int) = x
function evalexpr(e::Tuple)
    op, a, b = e
    op === :+ && return evalexpr(a) + evalexpr(b)
    op === :* && return evalexpr(a) * evalexpr(b)
    error("unknown expression")
end

abstract type Shape end
struct Circle <: Shape; r::Int; end
struct Rectangle <: Shape; w::Int; h::Int; end
area(s::Circle) = 3 * s.r * s.r
area(s::Rectangle) = s.w * s.h
perimeter(s::Circle) = 6 * s.r
perimeter(s::Rectangle) = 2 * (s.w + s.h)

struct Document; text::String; end
struct Row; id::Int; name::String; end

function command_example()
    commands = [10, -3]
    balance = foldl(+, commands; init=0)
    undone = balance - last(commands)
    balance == 7 && undone == 10
end

interpreter_example() = evalexpr((:+, 2, (:*, 3, 4))) == 14

function iterator_example()
    next(xs, cursor) = cursor <= length(xs) ? (xs[cursor], cursor + 1) : nothing
    next([10, 20], 1) == (10, 2) && next([10, 20], 3) === nothing
end

function mediator_example()
    mediate(sender, msg) = sender == "sales" ? ("billing", msg) : ("sales", msg)
    mediate("sales", "invoice") == ("billing", "invoice")
end

memento_example() = Document("v2").text == "v2" && Document("v1").text == "v1"

function observer_example()
    observers = [x -> "audit:$x", x -> "ui:$x"]
    map(f -> f(7), observers) == ["audit:7", "ui:7"]
end

function state_example()
    action(state) = state == :logged_out ? (:logged_in, "login") : (:logged_out, "logout")
    action(:logged_out) == (:logged_in, "login") && action(:logged_in) == (:logged_out, "logout")
end

function strategy_example()
    regular(x) = x
    discounted(x) = x * 80 ÷ 100
    regular(100) == 100 && discounted(100) == 80
end

function template_method_example()
    run(transform, input) = ["open", transform(input), "close"]
    run(reverse, "abc") == ["open", "cba", "close"]
end

function visitor_example()
    shapes = Shape[Circle(2), Rectangle(3, 4)]
    map(area, shapes) == [12, 12] && map(perimeter, shapes) == [12, 14]
end

function mvc_example()
    model = 3
    controller(m) = m + 1
    view(m) = "count=$m"
    view(controller(model)) == "count=4"
end

function mvvm_example()
    viewmodel(name, enabled) = ("Hello $name", enabled ? "enabled" : "disabled")
    viewmodel("Ada", true) == ("Hello Ada", "enabled")
end

function microkernel_example()
    plugins = Dict("double" => (x -> x * 2), "square" => (x -> x * x))
    plugins["double"](5) == 10
end

function microservices_example()
    inventory(sku) = sku == "A" ? 3 : 0
    pricing(sku) = sku == "A" ? 20 : 0
    (inventory("A"), pricing("A")) == (3, 20)
end

enterprise_adapter_example() = (dollars -> dollars * 100)(12) == 1200

function enterprise_bridge_example()
    render(transport, payload) = transport(payload)
    http(p) = "http:$p"
    queue(p) = "queue:$p"
    render(http, "x") == "http:x" && render(queue, "x") == "queue:x"
end

function enterprise_facade_example()
    validate(x) = x > 0
    persist(x) = "saved:$x"
    facade(x) = validate(x) ? persist(x) : "rejected"
    facade(5) == "saved:5"
end

function broker_example()
    registry = Dict("tax" => (x -> x * 16 ÷ 100))
    registry["tax"](100) == 16
end

function message_bus_example()
    handlers = [m -> "audit:$m", m -> "mail:$m"]
    map(h -> h("paid"), handlers) == ["audit:paid", "mail:paid"]
end

service_locator_example() = Dict("clock" => "12:00", "region" => "mx")["region"] == "mx"

function active_object_example()
    queue = String[]
    push!(queue, "sync")
    executed = "run:" * popfirst!(queue)
    executed == "run:sync" && isempty(queue)
end

function monitor_object_example()
    deposit(amount, balance) = balance + amount
    withdraw(amount, balance) = balance >= amount ? balance - amount : balance
    withdraw(7, deposit(10, 5)) == 8
end

function half_sync_half_async_example()
    queue = String[]
    push!(queue, "evt")
    processed = "processed:" * popfirst!(queue)
    processed == "processed:evt" && isempty(queue)
end

function leader_followers_example()
    pool = ["a", "b", "c"]
    leader = popfirst!(pool)
    push!(pool, leader)
    "$leader:evt" == "a:evt" && pool == ["b", "c", "a"]
end

function client_server_example()
    server(request) = "response($request)"
    client(request) = server(request)
    client("ping") == "response(ping)"
end

function peer_to_peer_example()
    send(from, to, payload) = "$from->$to:$payload"
    send("a", "b", "x") == "a->b:x" && send("b", "a", "y") == "b->a:y"
end

function publish_subscribe_example()
    subscriptions = Dict("orders" => ["audit", "warehouse"], "users" => ["crm"])
    subscriptions["orders"] == ["audit", "warehouse"]
end

function distributed_proxy_example()
    remote(id) = "remote-user-$id"
    proxy(id) = remote(id)
    proxy(7) == "remote-user-7"
end

function presentation_abstraction_control_example()
    abstraction = 4
    control(model, action) = action == :inc ? model + 1 : model
    presentation(model) = "value=$model"
    presentation(control(abstraction, :inc)) == "value=5"
end

function model_view_presenter_example()
    presenter(value) = "Hello $value"
    passive_view(text) = "[$text]"
    passive_view(presenter("Ada")) == "[Hello Ada]"
end

function document_view_example()
    doc = Document("hello")
    plain(d) = d.text
    upper(d) = uppercase(d.text)
    plain(doc) == "hello" && upper(doc) == "HELLO"
end

function active_record_example()
    store = Dict{Int,String}()
    save(row) = (store[row.id] = row.name)
    save(Row(1, "Ada"))
    store[1] == "Ada"
end

function data_mapper_example()
    to_row(r) = (r.id, r.name)
    from_row(tuple) = Row(tuple...)
    r = from_row(to_row(Row(1, "Ada")))
    r.id == 1 && r.name == "Ada"
end

function unit_of_work_example()
    pending = Tuple{Int,String}[]
    push!(pending, (1, "Ada"))
    store = copy(pending)
    store == [(1, "Ada")]
end

function repository_example()
    store = Dict{Int,String}()
    save(id, name) = (store[id] = name)
    find(id) = get(store, id, nothing)
    save(1, "Ada")
    find(1) == "Ada"
end

function dependency_injection_example()
    service(clock) = "time=" * clock()
    service(() -> "12:00") == "time=12:00"
end

function lazy_initialization_example()
    resource = Ref{Union{Nothing,String}}(nothing)
    created = Ref(0)
    function getresource()
        if isnothing(resource[])
            resource[] = "resource"
            created[] += 1
        end
        resource[]
    end
    getresource() == "resource" && getresource() == "resource" && created[] == 1
end

function object_pool_example()
    pool = ["c1", "c2"]
    resource = popfirst!(pool)
    push!(pool, resource)
    pool == ["c2", "c1"]
end

function null_object_example()
    run(logger, msg) = logger(msg)
    real(msg) = "log:$msg"
    null_logger(_) = ""
    run(real, "x") == "log:x" && run(null_logger, "x") == ""
end

const TESTS = [
    "Command" => command_example,
    "Interpreter" => interpreter_example,
    "Iterator" => iterator_example,
    "Mediator" => mediator_example,
    "Memento" => memento_example,
    "Observer" => observer_example,
    "State" => state_example,
    "Strategy" => strategy_example,
    "Template Method" => template_method_example,
    "Visitor" => visitor_example,
    "MVC" => mvc_example,
    "MVVM" => mvvm_example,
    "Microkernel" => microkernel_example,
    "Microservices" => microservices_example,
    "Enterprise Adapter" => enterprise_adapter_example,
    "Enterprise Bridge" => enterprise_bridge_example,
    "Enterprise Facade" => enterprise_facade_example,
    "Broker" => broker_example,
    "Message Bus" => message_bus_example,
    "Service Locator" => service_locator_example,
    "Active Object" => active_object_example,
    "Monitor Object" => monitor_object_example,
    "Half-Sync / Half-Async" => half_sync_half_async_example,
    "Leader / Followers" => leader_followers_example,
    "Client-Server" => client_server_example,
    "Peer-to-Peer" => peer_to_peer_example,
    "Publish-Subscribe" => publish_subscribe_example,
    "Distributed Proxy" => distributed_proxy_example,
    "Presentation-Abstraction-Control" => presentation_abstraction_control_example,
    "Model-View-Presenter" => model_view_presenter_example,
    "Document-View" => document_view_example,
    "Active Record" => active_record_example,
    "Data Mapper" => data_mapper_example,
    "Unit of Work" => unit_of_work_example,
    "Repository" => repository_example,
    "Dependency Injection" => dependency_injection_example,
    "Lazy Initialization" => lazy_initialization_example,
    "Object Pool" => object_pool_example,
    "Null Object" => null_object_example,
]

failed = [name for (name, test) in TESTS if !test()]
isempty(failed) || error("Julia pattern sweep failures: " * join(failed, ", "))
println("Julia pattern sweep: $(length(TESTS))/$(length(TESTS)) examples passed")
