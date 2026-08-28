abstract class Expression
  abstract def evaluate : Int32
end

class Literal < Expression
  def initialize(@value : Int32)
  end

  def evaluate : Int32
    @value
  end
end

class Add < Expression
  def initialize(@left : Expression, @right : Expression)
  end

  def evaluate : Int32
    @left.evaluate + @right.evaluate
  end
end

class Multiply < Expression
  def initialize(@left : Expression, @right : Expression)
  end

  def evaluate : Int32
    @left.evaluate * @right.evaluate
  end
end

def command_example : Bool
  commands = [10, -3]
  balance = commands.sum
  balance == 7 && balance - commands.last == 10
end

def interpreter_example : Bool
  Add.new(Literal.new(2), Multiply.new(Literal.new(3), Literal.new(4))).evaluate == 14
end

def iterator_example : Bool
  values = [10, 20]
  cursor = 0
  first = values[cursor]
  cursor += 1
  exhausted = cursor == values.size
  second = values[cursor]
  cursor += 1
  first == 10 && second == 20 && !exhausted && cursor == values.size
end

def mediator_example : Bool
  mediate = ->(sender : String, message : String) { sender == "sales" ? "billing:#{message}" : "sales:#{message}" }
  mediate.call("sales", "invoice") == "billing:invoice"
end

def memento_example : Bool
  current = "v1"
  snapshot = current
  current = "v2"
  current = snapshot
  current == "v1"
end

def observer_example : Bool
  observers = [
    ->(value : Int32) { "audit:#{value}" },
    ->(value : Int32) { "ui:#{value}" },
  ]
  observers.map(&.call(7)).join("|") == "audit:7|ui:7"
end

def state_example : Bool
  logged_in = false
  transition = -> do
    logged_in = !logged_in
    logged_in ? "login" : "logout"
  end
  transition.call == "login" && transition.call == "logout" && !logged_in
end

def strategy_example : Bool
  regular = ->(value : Int32) { value }
  discounted = ->(value : Int32) { value * 80 // 100 }
  regular.call(100) == 100 && discounted.call(100) == 80
end

def template_method_example : Bool
  run = ->(input : String, transform : Proc(String, String)) { "open|#{transform.call(input)}|close" }
  reverse = ->(input : String) { input.reverse }
  run.call("abc", reverse) == "open|cba|close"
end

def visitor_example : Bool
  shapes = [{kind: :circle, area: 12, perimeter: 12}, {kind: :rectangle, area: 12, perimeter: 14}]
  area_visitor = ->(shape : NamedTuple(kind: Symbol, area: Int32, perimeter: Int32)) { shape[:area] }
  perimeter_visitor = ->(shape : NamedTuple(kind: Symbol, area: Int32, perimeter: Int32)) { shape[:perimeter] }
  area_visitor.call(shapes.first) == 12 && perimeter_visitor.call(shapes.last) == 14
end

def mvc_example : Bool
  model = 3
  controller = -> { model += 1 }
  view = -> { "count=#{model}" }
  controller.call
  view.call == "count=4"
end

def mvvm_example : Bool
  view_model = ->(name : String, enabled : Bool) { {greeting: "Hello #{name}", state: enabled ? "enabled" : "disabled"} }
  vm = view_model.call("Ada", true)
  vm[:greeting] == "Hello Ada" && vm[:state] == "enabled"
end

def microkernel_example : Bool
  plugins = {
    "double" => ->(value : Int32) { value * 2 },
    "square" => ->(value : Int32) { value * value },
  }
  plugins["double"].call(5) == 10 && plugins["square"].call(3) == 9
end

def microservices_example : Bool
  inventory = ->(sku : String) { sku == "A" ? 3 : 0 }
  pricing = ->(sku : String) { sku == "A" ? 20 : 0 }
  inventory.call("A") == 3 && pricing.call("A") == 20
end

def enterprise_adapter_example : Bool
  legacy = ->(cents : Int32) { cents }
  adapt = ->(dollars : Int32) { legacy.call(dollars * 100) }
  adapt.call(12) == 1200
end

def enterprise_bridge_example : Bool
  render = ->(transport : Proc(String, String), payload : String) { transport.call(payload) }
  http = ->(payload : String) { "http:#{payload}" }
  queue = ->(payload : String) { "queue:#{payload}" }
  render.call(http, "x") == "http:x" && render.call(queue, "x") == "queue:x"
end

def enterprise_facade_example : Bool
  validate = ->(value : Int32) { value > 0 }
  persist = ->(value : Int32) { "saved:#{value}" }
  facade = ->(value : Int32) { validate.call(value) ? persist.call(value) : "rejected" }
  facade.call(5) == "saved:5" && facade.call(0) == "rejected"
end

def broker_example : Bool
  registry = {"tax" => ->(value : Int32) { value * 16 // 100 }}
  registry["tax"].call(100) == 16
end

def message_bus_example : Bool
  subscribers = [
    ->(message : String) { "audit:#{message}" },
    ->(message : String) { "mail:#{message}" },
  ]
  subscribers.map(&.call("paid")).join("|") == "audit:paid|mail:paid"
end

def service_locator_example : Bool
  services = {"clock" => "12:00", "region" => "mx"}
  services["region"] == "mx"
end

def active_object_example : Bool
  queue = [] of String
  queue << "sync"
  ran = "run:#{queue.shift}"
  ran == "run:sync" && queue.empty?
end

def monitor_object_example : Bool
  balance = 5
  deposit = ->(amount : Int32) { balance += amount }
  withdraw = ->(amount : Int32) do
    if balance >= amount
      balance -= amount
      true
    else
      false
    end
  end
  deposit.call(10)
  withdraw.call(7) && balance == 8
end

def half_sync_half_async_example : Bool
  async_queue = [] of String
  async_queue << "evt"
  processed = "processed:#{async_queue.shift}"
  processed == "processed:evt" && async_queue.empty?
end

def leader_followers_example : Bool
  pool = ["a", "b", "c"]
  leader = pool.shift
  pool << leader
  "#{leader}:evt" == "a:evt" && pool.join(",") == "b,c,a"
end

def client_server_example : Bool
  server = ->(request : String) { "response(#{request})" }
  client = ->(request : String) { server.call(request) }
  client.call("ping") == "response(ping)"
end

def peer_to_peer_example : Bool
  send = ->(from : String, to : String, payload : String) { "#{from}->#{to}:#{payload}" }
  send.call("a", "b", "x") == "a->b:x" && send.call("b", "a", "y") == "b->a:y"
end

def publish_subscribe_example : Bool
  subscriptions = {"orders" => ["audit", "warehouse"], "users" => ["crm"]}
  subscriptions["orders"].join(",") == "audit,warehouse"
end

def distributed_proxy_example : Bool
  remote = ->(id : Int32) { "remote-user-#{id}" }
  proxy = ->(id : Int32) { remote.call(id) }
  proxy.call(7) == "remote-user-7"
end

def presentation_abstraction_control_example : Bool
  abstraction = 4
  control = ->(action : String) { abstraction += 1 if action == "inc" }
  presentation = -> { "value=#{abstraction}" }
  control.call("inc")
  presentation.call == "value=5"
end

def model_view_presenter_example : Bool
  presenter = ->(value : String) { "Hello #{value}" }
  passive_view = ->(text : String) { "[#{text}]" }
  passive_view.call(presenter.call("Ada")) == "[Hello Ada]"
end

def document_view_example : Bool
  document = "hello"
  plain = ->(value : String) { value }
  upper = ->(value : String) { value.upcase }
  plain.call(document) == "hello" && upper.call(document) == "HELLO"
end

def active_record_example : Bool
  store = {} of Int32 => String
  save = ->(id : Int32, name : String) { store[id] = name }
  save.call(1, "Ada")
  store[1] == "Ada"
end

def data_mapper_example : Bool
  to_row = ->(record : NamedTuple(id: Int32, name: String)) { {id: record[:id], name: record[:name]} }
  from_row = ->(row : NamedTuple(id: Int32, name: String)) { {id: row[:id], name: row[:name]} }
  row = to_row.call({id: 1, name: "Ada"})
  from_row.call(row) == {id: 1, name: "Ada"}
end

def unit_of_work_example : Bool
  pending = [] of NamedTuple(id: Int32, name: String)
  store = [] of NamedTuple(id: Int32, name: String)
  pending << {id: 1, name: "Ada"}
  store.concat(pending)
  pending.clear
  store.size == 1 && store.first[:name] == "Ada" && pending.empty?
end

def repository_example : Bool
  store = {} of Int32 => String
  save = ->(id : Int32, name : String) { store[id] = name }
  find = ->(id : Int32) { store[id]? }
  save.call(1, "Ada")
  find.call(1) == "Ada"
end

def dependency_injection_example : Bool
  service = ->(clock : Proc(String)) { "time=#{clock.call}" }
  service.call(-> { "12:00" }) == "time=12:00"
end

def lazy_initialization_example : Bool
  resource = nil.as(String?)
  created = 0
  get_resource = -> do
    if resource.nil?
      resource = "resource"
      created += 1
    end
    resource.not_nil!
  end
  get_resource.call == "resource" && get_resource.call == "resource" && created == 1
end

def object_pool_example : Bool
  pool = ["c1", "c2"]
  resource = pool.shift
  pool << resource
  pool.join(",") == "c2,c1"
end

def null_object_example : Bool
  run = ->(logger : Proc(String, String), message : String) { logger.call(message) }
  real = ->(message : String) { "log:#{message}" }
  null = ->(_message : String) { "" }
  run.call(real, "x") == "log:x" && run.call(null, "x").empty?
end

checks = {
  "Command"                           => -> { command_example },
  "Interpreter"                       => -> { interpreter_example },
  "Iterator"                          => -> { iterator_example },
  "Mediator"                          => -> { mediator_example },
  "Memento"                           => -> { memento_example },
  "Observer"                          => -> { observer_example },
  "State"                             => -> { state_example },
  "Strategy"                          => -> { strategy_example },
  "Template Method"                   => -> { template_method_example },
  "Visitor"                           => -> { visitor_example },
  "MVC"                               => -> { mvc_example },
  "MVVM"                              => -> { mvvm_example },
  "Microkernel"                       => -> { microkernel_example },
  "Microservices"                     => -> { microservices_example },
  "Enterprise Adapter"                => -> { enterprise_adapter_example },
  "Enterprise Bridge"                 => -> { enterprise_bridge_example },
  "Enterprise Facade"                 => -> { enterprise_facade_example },
  "Broker"                            => -> { broker_example },
  "Message Bus"                       => -> { message_bus_example },
  "Service Locator"                   => -> { service_locator_example },
  "Active Object"                     => -> { active_object_example },
  "Monitor Object"                    => -> { monitor_object_example },
  "Half-Sync / Half-Async"            => -> { half_sync_half_async_example },
  "Leader / Followers"                => -> { leader_followers_example },
  "Client-Server"                     => -> { client_server_example },
  "Peer-to-Peer"                      => -> { peer_to_peer_example },
  "Publish-Subscribe"                 => -> { publish_subscribe_example },
  "Distributed Proxy"                 => -> { distributed_proxy_example },
  "Presentation-Abstraction-Control"  => -> { presentation_abstraction_control_example },
  "Model-View-Presenter"              => -> { model_view_presenter_example },
  "Document-View"                     => -> { document_view_example },
  "Active Record"                     => -> { active_record_example },
  "Data Mapper"                       => -> { data_mapper_example },
  "Unit of Work"                      => -> { unit_of_work_example },
  "Repository"                        => -> { repository_example },
  "Dependency Injection"              => -> { dependency_injection_example },
  "Lazy Initialization"               => -> { lazy_initialization_example },
  "Object Pool"                       => -> { object_pool_example },
  "Null Object"                       => -> { null_object_example },
}

failed = checks.select { |_name, check| !check.call }.keys
raise "Crystal pattern sweep failures: #{failed.join(", ")}" unless failed.empty?
puts "Crystal pattern sweep: #{checks.size}/#{checks.size} examples passed"
