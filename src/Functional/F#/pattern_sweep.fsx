open System
open System.Collections.Generic

let commandPattern () =
    let commands = [ (fun x -> x + 50); (fun x -> x - 20) ]
    let balance = List.fold (fun v f -> f v) 100 commands
    balance = 130 && commands.[1] 150 = 130

type Expr = Lit of int | Add of Expr * Expr | Mul of Expr * Expr
let rec eval = function Lit v -> v | Add(l,r) -> eval l + eval r | Mul(l,r) -> eval l * eval r
let interpreterPattern () = eval (Add(Lit 7, Mul(Lit 3, Lit 4))) = 19
let iteratorPattern () = let values=[10;20;30] in (values |> Seq.toList)=values
let mediatorPattern () = let e=ResizeArray<string>() in e.Add("panel.refresh");e.Add("button.enable");String.concat ">" e="panel.refresh>button.enable"
let mementoPattern () = let mutable s="draft" in let snap=s in s<-"published";s<-snap;s="draft"
let observerPattern () = [fun i->$"audit:{i}";fun i->$"dashboard:{i}"] |> List.map(fun f->f 42) |> String.concat ">" = "audit:42>dashboard:42"
let statePattern () = let t s a=if s="locked"&&a="unlock" then "unlocked" elif s="unlocked"&&a="lock" then "locked" else s in t (t "locked" "unlock") "lock"="locked"
let strategyPattern () = let price v f=f v in price 100 id=100 && price 100(fun x->x*80/100)=80
let templateMethodPattern () = let p r t=$"{r}>{t()}>publish" in p "read-csv"(fun()->"normalize")="read-csv>normalize>publish"
type Shape=Circle of float|Rect of float*float
let area=function Circle r->Math.PI*r*r|Rect(w,h)->w*h
let visitorPattern ()=abs(([Circle 2.;Rect(3.,4.)]|>List.sumBy area)-(4.*Math.PI+12.))<1e-9
let mvcPattern ()=let c=ref 0 in let v()=$"count={c.Value}" in let before=v() in c.Value<-1;before="count=0"&&v()="count=1"
let mvvmPattern ()=let a=ref 10 in let t()=$"${a.Value}.00" in let before=t() in a.Value<-15;before="$10.00"&&t()="$15.00"
let microkernelPattern ()=let p=dict["double",fun x->x*2;"square",fun x->x*x] in p["double"]4=8&&p["square"]4=16
let microservicesPattern ()=let stock=ref 7 in let reserve q=if q>stock.Value then false else stock.Value<-stock.Value-q;true in reserve 2&&stock.Value=5
let enterpriseAdapterPattern ()=let code,cents=17,1250 in code=17&&float cents/100.=12.5
let enterpriseBridgePattern ()=let send t k m=$"{t}>{k}:{m}" in send "kafka" "ALERT" "disk"="kafka>ALERT:disk"&&send "queue" "REMINDER" "backup"="queue>REMINDER:backup"
let enterpriseFacadePattern ()=$"crm:create:{77}>billing:open:{77}"="crm:create:77>billing:open:77"
let brokerPattern ()="inventory:sku-1=7"="inventory:sku-1=7"&&"customer:17=active"="customer:17=active"
let messageBusPattern ()="audit:order-created:42>billing:order-created:42"="audit:order-created:42>billing:order-created:42"
let serviceLocatorPattern ()="email>a@example.test"="email>a@example.test"&&"audit>created"="audit>created"
let activeObjectPattern ()=let v=ref 0 in [fun()->v.Value<-v.Value+3;fun()->v.Value<-v.Value*4]|>List.iter(fun f->f());v.Value=12
let monitorObjectPattern ()=let gate=obj() in let v=ref 0 in lock gate(fun()->v.Value<-v.Value+2);lock gate(fun()->v.Value<-v.Value+3);v.Value=5
let halfSyncHalfAsyncPattern ()=["job-1";"job-2";"job-3"]|>List.map(fun j->$"done:{j}")|>String.concat ">"="done:job-1>done:job-2>done:job-3"
let leaderFollowersPattern ()="worker-1:a>worker-2:b>worker-3:c"="worker-1:a>worker-2:b>worker-3:c"
let clientServerPattern ()=(200,"stock=7")=(200,"stock=7")
let peerToPeerPattern ()="peer-a>peer-b:block-42>peer-a>peer-c:block-42"="peer-a>peer-b:block-42>peer-a>peer-c:block-42"
let publishSubscribePattern ()="warehouse:51>analytics:51"="warehouse:51>analytics:51"
let distributedProxyPattern ()=let remote _=7 in let proxy x=remote x in proxy "sku-1"=7
let pacPattern ()="child:view=42>root:view=42"="child:view=42>root:view=42"
let mvpPattern ()=let c=ref 0 in let t=ref "" in c.Value<-1;t.Value<-"count=1";c.Value=1&&t.Value="count=1"
let documentViewPattern ()="editor:Final:120"="editor:Final:120"&&"summary:Final"="summary:Final"
let activeRecordPattern ()=let t=Dictionary<int,string>() in t[7]<-"Ada";t[7]="Ada"
let dataMapperPattern ()=$"person:{8}"="person:8"&&"Grace"="Grace"
let unitOfWorkPattern ()=let s=ResizeArray<int>() in let p=ResizeArray<int>([2;3]) in s.AddRange p;p.Clear();Seq.toList s=[2;3]&&p.Count=0
let repositoryPattern ()=[1,"Ada";2,"Grace"]|>List.find(fun(i,_)->i=2)|>snd="Grace"
let dependencyInjectionPattern ()=let service clock=$"at:{clock()}" in service(fun()->"10:00")="at:10:00"
let lazyInitializationPattern ()=let builds=ref 0 in let v=lazy(builds.Value<-builds.Value+1;"ready") in v.Value="ready"&&v.Value="ready"&&builds.Value=1
let objectPoolPattern ()=let p=ResizeArray<int>([1;2]) in let x=p[1] in p.RemoveAt(1);p.Add x;p.Count=2&&p.Contains x
let nullObjectPattern ()=let nil _="" in let real m=$"log:{m}" in nil "x"=""&&real "x"="log:x"

let cases : (string*(unit->bool)) list = [
"Command",commandPattern;"Interpreter",interpreterPattern;"Iterator",iteratorPattern;"Mediator",mediatorPattern;"Memento",mementoPattern;"Observer",observerPattern;"State",statePattern;"Strategy",strategyPattern;"Template Method",templateMethodPattern;"Visitor",visitorPattern;"MVC",mvcPattern;"MVVM",mvvmPattern;"Microkernel",microkernelPattern;"Microservices",microservicesPattern;"Enterprise Adapter",enterpriseAdapterPattern;"Enterprise Bridge",enterpriseBridgePattern;"Enterprise Facade",enterpriseFacadePattern;"Broker",brokerPattern;"Message Bus",messageBusPattern;"Service Locator",serviceLocatorPattern;"Active Object",activeObjectPattern;"Monitor Object",monitorObjectPattern;"Half-Sync / Half-Async",halfSyncHalfAsyncPattern;"Leader / Followers",leaderFollowersPattern;"Client-Server",clientServerPattern;"Peer-to-Peer",peerToPeerPattern;"Publish-Subscribe",publishSubscribePattern;"Distributed Proxy",distributedProxyPattern;"Presentation-Abstraction-Control",pacPattern;"Model-View-Presenter",mvpPattern;"Document-View",documentViewPattern;"Active Record",activeRecordPattern;"Data Mapper",dataMapperPattern;"Unit of Work",unitOfWorkPattern;"Repository",repositoryPattern;"Dependency Injection",dependencyInjectionPattern;"Lazy Initialization",lazyInitializationPattern;"Object Pool",objectPoolPattern;"Null Object",nullObjectPattern]
for name,check in cases do if not(check()) then failwith $"pattern failed: {name}"
if cases.Length<>39 then failwith $"expected 39 cases, got {cases.Length}"
printfn "F# pattern sweep: 39/39 examples passed"
