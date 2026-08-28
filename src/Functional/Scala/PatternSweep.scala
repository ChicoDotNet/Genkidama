import std/[tables, sequtils, strformat, strutils, math, locks]

proc commandPattern(): bool =
  let commands = [
    proc(x: int): int = x + 50,
    proc(x: int): int = x - 20
  ]
  var balance = 100
  for command in commands:
    balance = command(balance)
  balance == 130 and commands[1](150) == 130

type
  ExprKind = enum ekLit, ekAdd, ekMul
  Expr = ref object
    kind: ExprKind
    value: int
    left, right: Expr

proc eval(e: Expr): int =
  case e.kind
  of ekLit: e.value
  of ekAdd: eval(e.left) + eval(e.right)
  of ekMul: eval(e.left) * eval(e.right)

proc interpreterPattern(): bool =
  let expression = Expr(
    kind: ekAdd,
    left: Expr(kind: ekLit, value: 7),
    right: Expr(
      kind: ekMul,
      left: Expr(kind: ekLit, value: 3),
      right: Expr(kind: ekLit, value: 4)
    )
  )
  eval(expression) == 19

proc iteratorPattern(): bool =
  var seen: seq[int] = @[]
  for value in [10, 20, 30]:
    seen.add(value)
  seen == @[10, 20, 30]

proc mediatorPattern(): bool =
  var events: seq[string] = @[]
  proc notify(sender, event: string) =
    if sender == "button" and event == "click":
      events.add("panel.refresh")
    elif sender == "panel" and event == "loaded":
      events.add("button.enable")
  notify("button", "click")
  notify("panel", "loaded")
  events.join(">") == "panel.refresh>button.enable"

proc mementoPattern(): bool =
  var state = "draft"
  let snapshot = state
  state = "published"
  let published = state == "published"
  state = snapshot
  published and state == "draft"

proc observerPattern(): bool =
  let observers = [
    proc(id: int): string = "audit:" & $id,
    proc(id: int): string = "dashboard:" & $id
  ]
  observers.mapIt(it(42)).join(">") == "audit:42>dashboard:42"

proc statePattern(): bool =
  proc transition(state, action: string): string =
    if state == "locked" and action == "unlock":
      "unlocked"
    elif state == "unlocked" and action == "lock":
      "locked"
    else:
      state
  transition(transition("locked", "unlock"), "lock") == "locked"

proc strategyPattern(): bool =
  proc price(value: int, strategy: proc(x: int): int): int =
    strategy(value)
  price(100, proc(x: int): int = x) == 100 and
    price(100, proc(x: int): int = x * 80 div 100) == 80

proc templateMethodPattern(): bool =
  proc pipeline(readStep: string, transform: proc(): string): string =
    readStep & ">" & transform() & ">publish"
  pipeline("read-csv", proc(): string = "normalize") ==
    "read-csv>normalize>publish"

type
  ShapeKind = enum skCircle, skRect
  Shape = object
    case kind: ShapeKind
    of skCircle:
      radius: float
    of skRect:
      width, height: float

proc area(shape: Shape): float =
  case shape.kind
  of skCircle: PI * shape.radius * shape.radius
  of skRect: shape.width * shape.height

proc visitorPattern(): bool =
  let shapes = @[
    Shape(kind: skCircle, radius: 2.0),
    Shape(kind: skRect, width: 3.0, height: 4.0)
  ]
  abs(shapes.mapIt(area(it)).foldl(a + b) - (4.0 * PI + 12.0)) < 1.0e-9

proc mvcPattern(): bool =
  var count = 0
  proc view(): string = "count=" & $count
  let before = view()
  inc count
  before == "count=0" and view() == "count=1"

proc mvvmPattern(): bool =
  var amount = 10
  proc text(): string = "$" & $amount & ".00"
  let before = text()
  amount += 5
  before == "$10.00" and text() == "$15.00"

proc microkernelPattern(): bool =
  type Plugin = proc(x: int): int
  var plugins = initTable[string, Plugin]()
  plugins["double"] = proc(x: int): int = x * 2
  plugins["square"] = proc(x: int): int = x * x
  plugins["double"](4) == 8 and plugins["square"](4) == 16

proc microservicesPattern(): bool =
  var stock = 7
  proc reserve(quantity: int): bool =
    if quantity > stock:
      return false
    stock -= quantity
    true
  proc place(quantity: int): string =
    if reserve(quantity): "confirmed" else: "rejected"
  place(2) == "confirmed" and stock == 5

proc enterpriseAdapterPattern(): bool =
  let legacyCode = 17
  let legacyCents = 1250
  let canonicalId = legacyCode
  let canonicalAmount = legacyCents.float / 100.0
  canonicalId == 17 and canonicalAmount == 12.5

proc enterpriseBridgePattern(): bool =
  proc send(transport, kind, message: string): string =
    transport & ">" & kind & ":" & message
  send("kafka", "ALERT", "disk") == "kafka>ALERT:disk" and
    send("queue", "REMINDER", "backup") == "queue>REMINDER:backup"

proc enterpriseFacadePattern(): bool =
  proc crm(id: int): string = "crm:create:" & $id
  proc billing(id: int): string = "billing:open:" & $id
  crm(77) & ">" & billing(77) == "crm:create:77>billing:open:77"

proc brokerPattern(): bool =
  type Service = proc(key: string): string
  var services = initTable[string, Service]()
  services["inventory"] = proc(key: string): string = "inventory:" & key & "=7"
  services["customer"] = proc(key: string): string = "customer:" & key & "=active"
  services["inventory"]("sku-1") == "inventory:sku-1=7" and
    services["customer"]("17") == "customer:17=active"

proc messageBusPattern(): bool =
  let handlers = [
    proc(topic: string, id: int): string = "audit:" & topic & ":" & $id,
    proc(topic: string, id: int): string = "billing:" & topic & ":" & $id
  ]
  handlers.mapIt(it("order-created", 42)).join(">") ==
    "audit:order-created:42>billing:order-created:42"

proc serviceLocatorPattern(): bool =
  type Service = proc(value: string): string
  var services = initTable[string, Service]()
  services["email"] = proc(value: string): string = "email>" & value
  services["audit"] = proc(value: string): string = "audit>" & value
  services["email"]("a@example.test") == "email>a@example.test" and
    services["audit"]("created") == "audit>created"

proc activeObjectPattern(): bool =
  var value = 0
  let queue = [
    proc() = value += 3,
    proc() = value *= 4
  ]
  let before = value
  for command in queue:
    command()
  before == 0 and value == 12

proc monitorObjectPattern(): bool =
  var gate: Lock
  initLock(gate)
  var value = 0
  withLock gate:
    value += 2
  withLock gate:
    value += 3
  deinitLock(gate)
  value == 5

proc halfSyncHalfAsyncPattern(): bool =
  let asyncIngress = @["job-1", "job-2", "job-3"]
  let syncCore = asyncIngress.mapIt("done:" & it)
  syncCore.join(">") == "done:job-1>done:job-2>done:job-3"

proc leaderFollowersPattern(): bool =
  let workers = @["worker-1", "worker-2", "worker-3"]
  let events = @["a", "b", "c"]
  var handled: seq[string] = @[]
  for index, event in events:
    handled.add(workers[index mod workers.len] & ":" & event)
  handled.join(">") == "worker-1:a>worker-2:b>worker-3:c" and
    workers[events.len mod workers.len] == "worker-1"

proc clientServerPattern(): bool =
  proc server(key: string): tuple[status: int, body: string] =
    if key == "sku-1": (200, "stock=7") else: (404, "missing")
  server("sku-1") == (status: 200, body: "stock=7")

proc peerToPeerPattern(): bool =
  var inbox: seq[string] = @[]
  proc send(fromPeer, toPeer, data: string) =
    inbox.add(fromPeer & ">" & toPeer & ":" & data)
  send("peer-a", "peer-b", "block-42")
  send("peer-a", "peer-c", "block-42")
  inbox.join(">") == "peer-a>peer-b:block-42>peer-a>peer-c:block-42"

proc publishSubscribePattern(): bool =
  let subscribers = [
    proc(id: int): string = "warehouse:" & $id,
    proc(id: int): string = "analytics:" & $id
  ]
  subscribers.mapIt(it(51)).join(">") == "warehouse:51>analytics:51"

proc distributedProxyPattern(): bool =
  proc remote(sku: string): int =
    if sku == "sku-1": 7 else: 0
  proc proxy(sku: string): int = remote(sku)
  proxy("sku-1") == 7

proc pacPattern(): bool =
  proc view(name: string, value: int): string =
    name & ":view=" & $value
  view("child", 42) == "child:view=42" and
    view("root", 42) == "root:view=42"

proc mvpPattern(): bool =
  var count = 0
  var text = ""
  proc present() =
    inc count
    text = "count=" & $count
  present()
  count == 1 and text == "count=1"

proc documentViewPattern(): bool =
  let title = "Final"
  let words = 120
  let editor = "editor:" & title & ":" & $words
  let summary = "summary:" & title
  editor == "editor:Final:120" and summary == "summary:Final"

proc activeRecordPattern(): bool =
  var table = initTable[int, string]()
  table[7] = "Ada"
  table[7] == "Ada"

proc dataMapperPattern(): bool =
  let id = 8
  let name = "Grace"
  let key = &"person:{id}"
  key == "person:8" and name == "Grace"

proc unitOfWorkPattern(): bool =
  var store: seq[int] = @[]
  var pending = @[2, 3]
  store.add(pending)
  pending.setLen(0)
  store == @[2, 3] and pending.len == 0

proc repositoryPattern(): bool =
  let rows = {1: "Ada", 2: "Grace"}.toTable
  rows[2] == "Grace"

proc dependencyInjectionPattern(): bool =
  proc service(clock: proc(): string): string =
    "at:" & clock()
  service(proc(): string = "10:00") == "at:10:00"

proc lazyInitializationPattern(): bool =
  var builds = 0
  var cache = ""
  proc getValue(): string =
    if cache.len == 0:
      inc builds
      cache = "ready"
    cache
  getValue() == "ready" and getValue() == "ready" and builds == 1

proc objectPoolPattern(): bool =
  var pool = @[1, 2]
  let value = pool.pop()
  pool.add(value)
  pool.len == 2 and value in pool

proc nullObjectPattern(): bool =
  proc nullLog(_: string): string = ""
  proc realLog(message: string): string = "log:" & message
  nullLog("x") == "" and realLog("x") == "log:x"

type PatternCheck = proc(): bool

let cases: seq[(string, PatternCheck)] = @[
  ("Command", commandPattern),
  ("Interpreter", interpreterPattern),
  ("Iterator", iteratorPattern),
  ("Mediator", mediatorPattern),
  ("Memento", mementoPattern),
  ("Observer", observerPattern),
  ("State", statePattern),
  ("Strategy", strategyPattern),
  ("Template Method", templateMethodPattern),
  ("Visitor", visitorPattern),
  ("MVC", mvcPattern),
  ("MVVM", mvvmPattern),
  ("Microkernel", microkernelPattern),
  ("Microservices", microservicesPattern),
  ("Enterprise Adapter", enterpriseAdapterPattern),
  ("Enterprise Bridge", enterpriseBridgePattern),
  ("Enterprise Facade", enterpriseFacadePattern),
  ("Broker", brokerPattern),
  ("Message Bus", messageBusPattern),
  ("Service Locator", serviceLocatorPattern),
  ("Active Object", activeObjectPattern),
  ("Monitor Object", monitorObjectPattern),
  ("Half-Sync / Half-Async", halfSyncHalfAsyncPattern),
  ("Leader / Followers", leaderFollowersPattern),
  ("Client-Server", clientServerPattern),
  ("Peer-to-Peer", peerToPeerPattern),
  ("Publish-Subscribe", publishSubscribePattern),
  ("Distributed Proxy", distributedProxyPattern),
  ("Presentation-Abstraction-Control", pacPattern),
  ("Model-View-Presenter", mvpPattern),
  ("Document-View", documentViewPattern),
  ("Active Record", activeRecordPattern),
  ("Data Mapper", dataMapperPattern),
  ("Unit of Work", unitOfWorkPattern),
  ("Repository", repositoryPattern),
  ("Dependency Injection", dependencyInjectionPattern),
  ("Lazy Initialization", lazyInitializationPattern),
  ("Object Pool", objectPoolPattern),
  ("Null Object", nullObjectPattern)
]

for (name, check) in cases:
  doAssert check(), "pattern failed: " & name

doAssert cases.len == 39
echo "Nim pattern sweep: 39/39 examples passed"
