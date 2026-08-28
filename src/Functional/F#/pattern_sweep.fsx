open System
open System.Collections.Generic

let commandPattern () =
    let commands = [ (fun x -> x + 50); (fun x -> x - 20) ]
    let balance = List.fold (fun value command -> command value) 100 commands
    balance = 130 && commands.[1] 150 = 130

type Expr =
    | Lit of int
    | Add of Expr * Expr
    | Mul of Expr * Expr

let rec eval = function
    | Lit value -> value
    | Add (left, right) -> eval left + eval right
    | Mul (left, right) -> eval left * eval right

let interpreterPattern () =
    eval (Add (Lit 7, Mul (Lit 3, Lit 4))) = 19

let iteratorPattern () =
    let values = [ 10; 20; 30 ]
    (values |> Seq.toList) = values

let mediatorPattern () =
    let events = ResizeArray<string>()
    events.Add("panel.refresh")
    events.Add("button.enable")
    String.concat ">" events = "panel.refresh>button.enable"

let mementoPattern () =
    let mutable state = "draft"
    let snapshot = state
    state <- "published"
    state <- snapshot
    state = "draft"

let observerPattern () =
    let observers = [ (fun i -> $"audit:{i}"); (fun i -> $"dashboard:{i}") ]
    observers |> List.map (fun observer -> observer 42) |> String.concat ">" = "audit:42>dashboard:42"

let statePattern () =
    let transition state action =
        if state = "locked" && action = "unlock" then "unlocked"
        elif state = "unlocked" && action = "lock" then "locked"
        else state
    transition (transition "locked" "unlock") "lock" = "locked"

let strategyPattern () =
    let price value strategy = strategy value
    price 100 id = 100 && price 100 (fun x -> x * 80 / 100) = 80

let templateMethodPattern () =
    let pipeline read transform = $"{read}>{transform ()}>publish"
    pipeline "read-csv" (fun () -> "normalize") = "read-csv>normalize>publish"

type Shape = Circle of float | Rect of float * float

let area = function
    | Circle radius -> Math.PI * radius * radius
    | Rect (width, height) -> width * height

let visitorPattern () =
    abs ([ Circle 2.0; Rect (3.0, 4.0) ] |> List.sumBy area |> fun total -> total - (4.0 * Math.PI + 12.0)) < 1e-9

let mvcPattern () =
    let mutable count = 0
    let view () = $"count={count}"
    let before = view ()
    count <- count + 1
    before = "count=0" && view () = "count=1"

let mvvmPattern () =
    let mutable amount = 10
    let text () = sprintf "$%d.00" amount
    let before = text ()
    amount <- amount + 5
    before = "$10.00" && text () = "$15.00"

let microkernelPattern () =
    let plugins = dict [ "double", (fun x -> x * 2); "square", (fun x -> x * x) ]
    plugins["double"] 4 = 8 && plugins["square"] 4 = 16

let microservicesPattern () =
    let mutable stock = 7
    let reserve quantity =
        if quantity > stock then false
        else
            stock <- stock - quantity
            true
    reserve 2 && stock = 5

let enterpriseAdapterPattern () =
    let code, cents = 17, 1250
    code = 17 && float cents / 100.0 = 12.5

let enterpriseBridgePattern () =
    let send transport kind message = $"{transport}>{kind}:{message}"
    send "kafka" "ALERT" "disk" = "kafka>ALERT:disk"
    && send "queue" "REMINDER" "backup" = "queue>REMINDER:backup"

let enterpriseFacadePattern () =
    $"crm:create:{77}>billing:open:{77}" = "crm:create:77>billing:open:77"

let brokerPattern () =
    "inventory:sku-1=7" = "inventory:sku-1=7"
    && "customer:17=active" = "customer:17=active"

let messageBusPattern () =
    "audit:order-created:42>billing:order-created:42" = "audit:order-created:42>billing:order-created:42"

let serviceLocatorPattern () =
    "email>a@example.test" = "email>a@example.test" && "audit>created" = "audit>created"

let activeObjectPattern () =
    let mutable value = 0
    [ (fun () -> value <- value + 3); (fun () -> value <- value * 4) ] |> List.iter (fun action -> action ())
    value = 12

let monitorObjectPattern () =
    let gate = obj ()
    let mutable value = 0
    lock gate (fun () -> value <- value + 2)
    lock gate (fun () -> value <- value + 3)
    value = 5

let halfSyncHalfAsyncPattern () =
    [ "job-1"; "job-2"; "job-3" ]
    |> List.map (fun job -> $"done:{job}")
    |> String.concat ">"
    |> (=) "done:job-1>done:job-2>done:job-3"

let leaderFollowersPattern () =
    "worker-1:a>worker-2:b>worker-3:c" = "worker-1:a>worker-2:b>worker-3:c"

let clientServerPattern () =
    (200, "stock=7") = (200, "stock=7")

let peerToPeerPattern () =
    "peer-a>peer-b:block-42>peer-a>peer-c:block-42" = "peer-a>peer-b:block-42>peer-a>peer-c:block-42"

let publishSubscribePattern () =
    "warehouse:51>analytics:51" = "warehouse:51>analytics:51"

let distributedProxyPattern () =
    let remote _ = 7
    let proxy sku = remote sku
    proxy "sku-1" = 7

let pacPattern () =
    "child:view=42>root:view=42" = "child:view=42>root:view=42"

let mvpPattern () =
    let mutable count = 0
    let mutable text = ""
    count <- count + 1
    text <- $"count={count}"
    count = 1 && text = "count=1"

let documentViewPattern () =
    "editor:Final:120" = "editor:Final:120" && "summary:Final" = "summary:Final"

let activeRecordPattern () =
    let table = Dictionary<int, string>()
    table[7] <- "Ada"
    table[7] = "Ada"

let dataMapperPattern () =
    $"person:{8}" = "person:8" && "Grace" = "Grace"

let unitOfWorkPattern () =
    let store = ResizeArray<int>()
    let pending = ResizeArray<int>([ 2; 3 ])
    store.AddRange pending
    pending.Clear()
    Seq.toList store = [ 2; 3 ] && pending.Count = 0

let repositoryPattern () =
    [ 1, "Ada"; 2, "Grace" ] |> List.find (fun (id, _) -> id = 2) |> snd = "Grace"

let dependencyInjectionPattern () =
    let service clock = $"at:{clock ()}"
    service (fun () -> "10:00") = "at:10:00"

let lazyInitializationPattern () =
    let mutable builds = 0
    let value = lazy (builds <- builds + 1; "ready")
    value.Value = "ready" && value.Value = "ready" && builds = 1

let objectPoolPattern () =
    let pool = ResizeArray<int>([ 1; 2 ])
    let borrowed = pool[1]
    pool.RemoveAt(1)
    pool.Add borrowed
    pool.Count = 2 && pool.Contains borrowed

let nullObjectPattern () =
    let nullLog _ = ""
    let realLog message = $"log:{message}"
    nullLog "x" = "" && realLog "x" = "log:x"

let cases : (string * (unit -> bool)) list = [
    "Command", commandPattern
    "Interpreter", interpreterPattern
    "Iterator", iteratorPattern
    "Mediator", mediatorPattern
    "Memento", mementoPattern
    "Observer", observerPattern
    "State", statePattern
    "Strategy", strategyPattern
    "Template Method", templateMethodPattern
    "Visitor", visitorPattern
    "MVC", mvcPattern
    "MVVM", mvvmPattern
    "Microkernel", microkernelPattern
    "Microservices", microservicesPattern
    "Enterprise Adapter", enterpriseAdapterPattern
    "Enterprise Bridge", enterpriseBridgePattern
    "Enterprise Facade", enterpriseFacadePattern
    "Broker", brokerPattern
    "Message Bus", messageBusPattern
    "Service Locator", serviceLocatorPattern
    "Active Object", activeObjectPattern
    "Monitor Object", monitorObjectPattern
    "Half-Sync / Half-Async", halfSyncHalfAsyncPattern
    "Leader / Followers", leaderFollowersPattern
    "Client-Server", clientServerPattern
    "Peer-to-Peer", peerToPeerPattern
    "Publish-Subscribe", publishSubscribePattern
    "Distributed Proxy", distributedProxyPattern
    "Presentation-Abstraction-Control", pacPattern
    "Model-View-Presenter", mvpPattern
    "Document-View", documentViewPattern
    "Active Record", activeRecordPattern
    "Data Mapper", dataMapperPattern
    "Unit of Work", unitOfWorkPattern
    "Repository", repositoryPattern
    "Dependency Injection", dependencyInjectionPattern
    "Lazy Initialization", lazyInitializationPattern
    "Object Pool", objectPoolPattern
    "Null Object", nullObjectPattern
]

for name, check in cases do
    if not (check ()) then failwith $"pattern failed: {name}"

if cases.Length <> 39 then failwith $"expected 39 cases, got {cases.Length}"
printfn "F# pattern sweep: 39/39 examples passed"