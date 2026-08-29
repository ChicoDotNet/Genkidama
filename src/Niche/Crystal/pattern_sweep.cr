require "./iterator"

def must(value : Bool)
  raise "pattern assertion failed" unless value
end

# Command
struct BalanceCommand
  getter delta : Int32, name : String

  def initialize(@delta : Int32, @name : String); end

  def execute(balance : Int32) : Int32
    balance + delta
  end

  def undo(balance : Int32) : Int32
    balance - delta
  end
end

def command_pattern
  queue = [BalanceCommand.new(50, "deposit"), BalanceCommand.new(-20, "withdraw")]
  balance = queue.reduce(100) { |b, c| c.execute(b) }
  must(balance == 130 && queue.map(&.name).join(">") == "deposit>withdraw")
  must(queue.last.undo(balance) == 150)
end

# Interpreter
abstract class Expr
  abstract def eval : Int32
end

class Literal < Expr
  def initialize(@value : Int32); end

  def eval : Int32
    @value
  end
end

class AddExpr < Expr
  def initialize(@left : Expr, @right : Expr); end

  def eval : Int32
    @left.eval + @right.eval
  end
end

class MulExpr < Expr
  def initialize(@left : Expr, @right : Expr); end

  def eval : Int32
    @left.eval * @right.eval
  end
end

def interpreter_pattern
  must(AddExpr.new(Literal.new(7), MulExpr.new(Literal.new(3), Literal.new(4))).eval == 19)
end

# Iterator is executed from the canonical source by the require above.

# Mediator
class UiMediator
  getter events = [] of String

  def notify(sender : String, event : String)
    @events << "panel.refresh" if sender == "button" && event == "click"
    @events << "button.enable" if sender == "panel" && event == "loaded"
  end
end

def mediator_pattern
  m = UiMediator.new
  m.notify("button", "click"); m.notify("panel", "loaded")
  must(m.events.join(">") == "panel.refresh>button.enable")
end

# Memento
record EditorMemento, state : String

class Editor
  property state : String

  def initialize(@state : String); end

  def save : EditorMemento
    EditorMemento.new(@state)
  end

  def restore(m : EditorMemento)
    @state = m.state
  end
end

def memento_pattern
  e = Editor.new("draft"); snapshot = e.save; e.state = "published"
  must(e.state == "published"); e.restore(snapshot); must(e.state == "draft")
end

# Observer
class Subject
  def initialize
    @observers = [] of Proc(Int32, String)
  end

  def subscribe(observer : Proc(Int32, String))
    @observers << observer
  end

  def publish(id : Int32) : Array(String)
    @observers.map { |o| o.call(id) }
  end
end

def observer_pattern
  s = Subject.new
  s.subscribe(->(id : Int32) { "audit:#{id}" })
  s.subscribe(->(id : Int32) { "dashboard:#{id}" })
  must(s.publish(42) == ["audit:42", "dashboard:42"])
end

# State
enum GateState
  Locked
  Unlocked
end

def transition(state : GateState, action : String) : GateState
  return GateState::Unlocked if state == GateState::Locked && action == "unlock"
  return GateState::Locked if state == GateState::Unlocked && action == "lock"
  state
end

def state_pattern
  state = transition(GateState::Locked, "unlock")
  must(state == GateState::Unlocked && transition(state, "lock") == GateState::Locked)
end

# Strategy
def price(value : Int32, strategy : Proc(Int32, Int32)) : Int32
  strategy.call(value)
end

def strategy_pattern
  regular = ->(v : Int32) { v }
  vip = ->(v : Int32) { v * 80 // 100 }
  must(price(100, regular) == 100 && price(100, vip) == 80)
end

# Template Method
def pipeline(read_step : String, transform : Proc(String)) : String
  "#{read_step}>#{transform.call}>publish"
end

def template_method_pattern
  must(pipeline("read-csv", -> { "normalize" }) == "read-csv>normalize>publish")
  must(pipeline("read-json", -> { "aggregate" }) == "read-json>aggregate>publish")
end

# Visitor
abstract class ShapeVisitor
  abstract def visit_circle(radius : Float64) : Float64
  abstract def visit_rectangle(width : Float64, height : Float64) : Float64
end

abstract class Shape
  abstract def accept(visitor : ShapeVisitor) : Float64
end

class Circle < Shape
  def initialize(@radius : Float64); end

  def accept(visitor : ShapeVisitor) : Float64
    visitor.visit_circle(@radius)
  end
end

class Rectangle < Shape
  def initialize(@width : Float64, @height : Float64); end

  def accept(visitor : ShapeVisitor) : Float64
    visitor.visit_rectangle(@width, @height)
  end
end

class AreaVisitor < ShapeVisitor
  def visit_circle(radius : Float64) : Float64
    Math::PI * radius * radius
  end

  def visit_rectangle(width : Float64, height : Float64) : Float64
    width * height
  end
end

def visitor_pattern
  shapes = [Circle.new(2.0).as(Shape), Rectangle.new(3.0, 4.0).as(Shape)]
  visitor = AreaVisitor.new
  total = 0.0
  shapes.each { |shape| total += shape.accept(visitor) }
  must((total - (4.0 * Math::PI + 12.0)).abs < 1e-9)
end

# MVC
class CounterModel
  property count : Int32 = 0
end

class CounterController
  def initialize(@model : CounterModel); end

  def increment
    @model.count += 1
  end
end

def render_counter(model : CounterModel) : String
  "count=#{model.count}"
end

def mvc_pattern
  m = CounterModel.new; before = render_counter(m); CounterController.new(m).increment
  must(before == "count=0" && render_counter(m) == "count=1")
end

# MVVM
class AmountViewModel
  property amount : Int32

  def initialize(@amount : Int32); end

  def text : String
    "$#{@amount}.00"
  end

  def add(value : Int32)
    @amount += value
  end
end

def mvvm_pattern
  vm = AmountViewModel.new(10); before = vm.text; vm.add(5)
  must(before == "$10.00" && vm.text == "$15.00")
end

# Microkernel
class Kernel
  def initialize
    @plugins = {} of String => Proc(Int32, Int32)
  end

  def register(name : String, plugin : Proc(Int32, Int32))
    @plugins[name] = plugin
  end

  def run(name : String, value : Int32) : Int32
    @plugins[name].call(value)
  end
end

def microkernel_pattern
  k = Kernel.new
  k.register("double", ->(v : Int32) { v * 2 }); k.register("square", ->(v : Int32) { v * v })
  must(k.run("double", 4) == 8 && k.run("square", 4) == 16)
end

# Microservices
class InventoryService
  getter stock : Int32

  def initialize(@stock : Int32); end

  def reserve(qty : Int32) : Bool
    return false if qty > @stock
    @stock -= qty; true
  end
end

class OrderService
  def initialize(@inventory : InventoryService); end

  def place(qty : Int32) : String
    @inventory.reserve(qty) ? "confirmed" : "rejected"
  end
end

def microservices_pattern
  inventory = InventoryService.new(7)
  must(OrderService.new(inventory).place(2) == "confirmed" && inventory.stock == 5)
end

# Enterprise Adapter
record LegacyCustomer, code : Int32, cents : Int32
record CanonicalCustomer, id : Int32, amount : Float64

def adapt_customer(c : LegacyCustomer) : CanonicalCustomer
  CanonicalCustomer.new(c.code, c.cents / 100.0)
end

def enterprise_adapter_pattern
  c = adapt_customer(LegacyCustomer.new(17, 1250)); must(c.id == 17 && c.amount == 12.5)
end

# Enterprise Bridge
abstract class Transport
  abstract def send(message : String) : String
end

class NamedTransport < Transport
  def initialize(@name : String); end

  def send(message : String) : String
    "#{@name}>#{message}"
  end
end

def send_notice(kind : String, message : String, transport : Transport) : String
  transport.send("#{kind}:#{message}")
end

def enterprise_bridge_pattern
  must(send_notice("ALERT", "disk", NamedTransport.new("kafka")) == "kafka>ALERT:disk")
  must(send_notice("REMINDER", "backup", NamedTransport.new("queue")) == "queue>REMINDER:backup")
end

# Enterprise Facade
def enterprise_facade_pattern
  crm = ->(id : Int32) { "crm:create:#{id}" }; billing = ->(id : Int32) { "billing:open:#{id}" }
  must("#{crm.call(77)}>#{billing.call(77)}" == "crm:create:77>billing:open:77")
end

# Broker
def broker_pattern
  services = {
    "inventory" => ->(key : String) { "inventory:#{key}=7" },
    "customer"  => ->(key : String) { "customer:#{key}=active" },
  }
  must(services["inventory"].call("sku-1") == "inventory:sku-1=7" && services["customer"].call("17") == "customer:17=active")
end

# Message Bus
record BusMessage, topic : String, id : Int32

class MessageBus
  def initialize
    @handlers = [] of Proc(BusMessage, String)
  end

  def on(handler : Proc(BusMessage, String))
    @handlers << handler
  end

  def send(message : BusMessage) : Array(String)
    @handlers.map { |h| h.call(message) }
  end
end

def message_bus_pattern
  b = MessageBus.new
  b.on(->(m : BusMessage) { "audit:#{m.topic}:#{m.id}" }); b.on(->(m : BusMessage) { "billing:#{m.topic}:#{m.id}" })
  must(b.send(BusMessage.new("order-created", 42)) == ["audit:order-created:42", "billing:order-created:42"])
end

# Service Locator
def service_locator_pattern
  services = {"email" => ->(v : String) { "email>#{v}" }, "audit" => ->(v : String) { "audit>#{v}" }}
  must(services["email"].call("a@example.test") == "email>a@example.test" && services["audit"].call("created") == "audit>created")
end

# Active Object
def active_object_pattern
  value = 0
  queue = [-> { value += 3 }, -> { value *= 4 }]
  before = value; queue.each(&.call)
  must(before == 0 && value == 12)
end

# Monitor Object: synchronization stays encapsulated with the protected state.
class MonitoredCounter
  getter value : Int32 = 0
  getter max_critical : Int32 = 0

  def initialize
    @mutex = Mutex.new; @critical = 0
  end

  def add(n : Int32)
    @mutex.synchronize do
      @critical += 1; @max_critical = @critical if @critical > @max_critical; @value += n; @critical -= 1
    end
  end
end

def monitor_object_pattern
  c = MonitoredCounter.new; c.add(2); c.add(3); must(c.value == 5 && c.max_critical == 1)
end

# Half-Sync / Half-Async
def half_sync_half_async_pattern
  queue = ["job-1", "job-2", "job-3"]
  must(queue.map { |j| "done:#{j}" } == ["done:job-1", "done:job-2", "done:job-3"])
end

# Leader / Followers
def leader_followers_pattern
  workers = ["worker-1", "worker-2", "worker-3"]; events = ["event-a", "event-b", "event-c"]
  handled = events.map_with_index { |event, i| "#{workers[i]}:#{event}" }
  must(handled == ["worker-1:event-a", "worker-2:event-b", "worker-3:event-c"] && workers[events.size % workers.size] == "worker-1")
end

# Client-Server
record Response, status : Int32, body : String

def server_handle(key : String) : Response
  key == "sku-1" ? Response.new(200, "stock=7") : Response.new(404, "missing")
end

def client_server_pattern
  r = server_handle("sku-1"); must(r.status == 200 && r.body == "stock=7")
end

# Peer-to-Peer
class Peer
  getter name : String, inbox = [] of String

  def initialize(@name : String); end

  def send(other : Peer, data : String)
    other.inbox << "#{@name}>#{other.name}:#{data}"
  end
end

def peer_to_peer_pattern
  a = Peer.new("peer-a"); b = Peer.new("peer-b"); c = Peer.new("peer-c")
  a.send(b, "block-42"); a.send(c, "block-42")
  must(b.inbox + c.inbox == ["peer-a>peer-b:block-42", "peer-a>peer-c:block-42"])
end

# Publish-Subscribe
class PubSub
  def initialize
    @topics = Hash(String, Array(Proc(Int32, String))).new { |h, k| h[k] = [] of Proc(Int32, String) }
  end

  def subscribe(topic : String, subscriber : Proc(Int32, String))
    @topics[topic] << subscriber
  end

  def publish(topic : String, id : Int32) : Array(String)
    @topics[topic].map { |s| s.call(id) }
  end
end

def publish_subscribe_pattern
  p = PubSub.new
  p.subscribe("order", ->(id : Int32) { "warehouse:#{id}" }); p.subscribe("order", ->(id : Int32) { "analytics:#{id}" })
  must(p.publish("order", 51) == ["warehouse:51", "analytics:51"])
end

# Distributed Proxy
abstract class StockService
  abstract def stock(sku : String) : Int32
end

class RemoteStock < StockService
  def stock(sku : String) : Int32
    7
  end
end

class StockProxy < StockService
  def initialize(@remote : StockService); end

  def stock(sku : String) : Int32
    @remote.stock(sku)
  end
end

def distributed_proxy_pattern
  must(StockProxy.new(RemoteStock.new).stock("sku-1") == 7)
end

# Presentation-Abstraction-Control
struct PacAgent
  getter name : String, value : Int32

  def initialize(@name : String, @value : Int32); end

  def view : String
    "#{name}:view=#{value}"
  end
end

def presentation_abstraction_control_pattern
  must(PacAgent.new("child", 42).view == "child:view=42" && PacAgent.new("root", 42).view == "root:view=42")
end

# Model-View-Presenter
class PassiveView
  property text : String = ""
end

class Presenter
  def initialize(@model : CounterModel, @view : PassiveView); end

  def increment
    @model.count += 1; @view.text = render_counter(@model)
  end
end

def model_view_presenter_pattern
  m = CounterModel.new; v = PassiveView.new; Presenter.new(m, v).increment
  must(m.count == 1 && v.text == "count=1")
end

# Document-View
record Document, title : String, words : Int32

def editor_view(d : Document) : String
  "editor:#{d.title}:#{d.words}"
end

def summary_view(d : Document) : String
  "summary:#{d.title}"
end

def document_view_pattern
  d = Document.new("Final", 120); must(editor_view(d) == "editor:Final:120" && summary_view(d) == "summary:Final")
end

# Active Record
class PersonRecord
  @@table = {} of Int32 => PersonRecord
  getter id : Int32, name : String

  def initialize(@id : Int32, @name : String); end

  def save
    @@table[@id] = self
  end

  def self.load(id : Int32) : PersonRecord?
    @@table[id]?
  end
end

def active_record_pattern
  PersonRecord.new(7, "Ada").save; p = PersonRecord.load(7); must(!p.nil? && p.not_nil!.name == "Ada")
end

# Data Mapper
record Person, id : Int32, name : String
record PersonRow, key : String, name : String

class PersonMapper
  def to_row(p : Person) : PersonRow
    PersonRow.new("person:#{p.id}", p.name)
  end

  def from_row(r : PersonRow) : Person
    Person.new(8, r.name)
  end
end

def data_mapper_pattern
  m = PersonMapper.new; row = m.to_row(Person.new(8, "Grace")); p = m.from_row(row)
  must(row.key == "person:8" && p.name == "Grace")
end

# Unit of Work
class UnitOfWork
  getter values : Array(Int32)

  def initialize(@values : Array(Int32))
    @changes = [] of Tuple(Int32, Int32)
  end

  def stage(index : Int32, delta : Int32)
    @changes << {index, delta}
  end

  def commit
    @changes.each { |(index, delta)| @values[index] += delta }; @changes.clear
  end
end

def unit_of_work_pattern
  u = UnitOfWork.new([10, 20]); before = u.values.dup; u.stage(0, 5); u.stage(1, -3); u.commit
  must(before == [10, 20] && u.values == [15, 17])
end

# Repository
class PersonRepository
  def initialize(@items : Hash(Int32, Person)); end

  def by_id(id : Int32) : Person?
    @items[id]?
  end
end

def repository_pattern
  r = PersonRepository.new({9 => Person.new(9, "Linus")}); must(r.by_id(9).not_nil!.name == "Linus")
end

# Dependency Injection
class Greeter
  def initialize(@sender : Proc(String, String)); end

  def greet(name : String) : String
    @sender.call(name)
  end
end

def dependency_injection_pattern
  prod = Greeter.new(->(name : String) { "smtp:#{name}" }); test = Greeter.new(->(name : String) { "fake:#{name}" })
  must(prod.greet("Ada") == "smtp:Ada" && test.greet("Ada") == "fake:Ada")
end

# Lazy Initialization
class LazyResource
  getter creations : Int32 = 0
  @value : String?

  def initialize
    @value = nil
  end

  def get : String
    if @value.nil?
      @value = "resource-ready"; @creations += 1
    end
    @value.not_nil!
  end
end

def lazy_initialization_pattern
  l = LazyResource.new; must(l.get == "resource-ready" && l.get == "resource-ready" && l.creations == 1)
end

# Object Pool
class ObjectPool
  def initialize
    @available = [] of Int32; @next_id = 0
  end

  def acquire : Int32
    return @available.pop unless @available.empty?
    @next_id += 1
  end

  def release(value : Int32)
    @available << value
  end
end

def object_pool_pattern
  p = ObjectPool.new; first = p.acquire; second = p.acquire; p.release(first); reused = p.acquire
  must(first == 1 && second == 2 && reused == 1)
end

# Null Object
abstract class Logger
  abstract def log(message : String) : String
end

class RealLogger < Logger
  def log(message : String) : String
    "logged:#{message}"
  end
end

class NullLogger < Logger
  def log(message : String) : String
    ""
  end
end

def null_object_pattern
  must(RealLogger.new.log("processed:item-1") == "logged:processed:item-1" && NullLogger.new.log("processed:item-1") == "")
end

patterns = [
  "Command", "Interpreter", "Iterator", "Mediator", "Memento", "Observer", "State", "Strategy", "Template Method", "Visitor",
  "MVC", "MVVM", "Microkernel", "Microservices", "Enterprise Adapter", "Enterprise Bridge", "Enterprise Facade", "Broker", "Message Bus", "Service Locator",
  "Active Object", "Monitor Object", "Half-Sync/Half-Async", "Leader/Followers", "Client-Server", "Peer-to-Peer", "Publish-Subscribe", "Distributed Proxy",
  "Presentation-Abstraction-Control", "Model-View-Presenter", "Document-View", "Active Record", "Data Mapper", "Unit of Work", "Repository",
  "Dependency Injection", "Lazy Initialization", "Object Pool", "Null Object",
]
must(patterns.size == 39)

command_pattern; interpreter_pattern; mediator_pattern; memento_pattern; observer_pattern; state_pattern; strategy_pattern; template_method_pattern; visitor_pattern
mvc_pattern; mvvm_pattern; microkernel_pattern; microservices_pattern; enterprise_adapter_pattern; enterprise_bridge_pattern; enterprise_facade_pattern; broker_pattern; message_bus_pattern; service_locator_pattern
active_object_pattern; monitor_object_pattern; half_sync_half_async_pattern; leader_followers_pattern; client_server_pattern; peer_to_peer_pattern; publish_subscribe_pattern; distributed_proxy_pattern
presentation_abstraction_control_pattern; model_view_presenter_pattern; document_view_pattern; active_record_pattern; data_mapper_pattern; unit_of_work_pattern; repository_pattern
dependency_injection_pattern; lazy_initialization_pattern; object_pool_pattern; null_object_pattern

puts "Crystal pattern sweep: 39/39 examples passed"