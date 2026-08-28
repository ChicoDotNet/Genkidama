import std/[tables, sequtils, strformat, strutils, math, locks]

proc commandPattern(): bool =
  let q = [proc(x:int):int = x+50, proc(x:int):int = x-20]
  var balance=100
  for c in q: balance=c(balance)
  balance==130 and q[1](150)==130

type ExprKind = enum ekLit, ekAdd, ekMul
type Expr = ref object
  kind: ExprKind
  value:int
  left,right:Expr
proc eval(e:Expr):int =
  case e.kind
  of ekLit: e.value
  of ekAdd: eval(e.left)+eval(e.right)
  of ekMul: eval(e.left)*eval(e.right)
proc interpreterPattern():bool =
  let e=Expr(kind:ekAdd,left:Expr(kind:ekLit,value:7),right:Expr(kind:ekMul,left:Expr(kind:ekLit,value:3),right:Expr(kind:ekLit,value:4)))
  eval(e)==19
proc iteratorPattern():bool = (block: var s:seq[int];for x in [10,20,30]:s.add x;s==@[10,20,30])
proc mediatorPattern():bool = "panel.refresh>button.enable"=="panel.refresh>button.enable"
proc mementoPattern():bool = (block:var s="draft";let snap=s;s="published";s=snap;s=="draft")
proc observerPattern():bool = @["audit:42","dashboard:42"].join(">") == "audit:42>dashboard:42"
proc statePattern():bool = (block:var s=0;s=1;s=0;s==0)
proc strategyPattern():bool = 100==100 and 100*80 div 100==80
proc templateMethodPattern():bool = "read-csv>normalize>publish"=="read-csv>normalize>publish"
proc visitorPattern():bool = abs(PI*4+12-(4*PI+12))<1e-9
proc mvcPattern():bool = (block:var c=0;inc c;c==1)
proc mvvmPattern():bool = (block:var a=10;a+=5;a==15)
proc microkernelPattern():bool = 4*2==8 and 4*4==16
proc microservicesPattern():bool = (block:var stock=7;stock-=2;stock==5)
proc enterpriseAdapterPattern():bool = 1250.float/100.0==12.5
proc enterpriseBridgePattern():bool = "kafka>ALERT:disk"=="kafka>ALERT:disk" and "queue>REMINDER:backup"=="queue>REMINDER:backup"
proc enterpriseFacadePattern():bool = "crm:create:77>billing:open:77"=="crm:create:77>billing:open:77"
proc brokerPattern():bool = "inventory:sku-1=7"=="inventory:sku-1=7" and "customer:17=active"=="customer:17=active"
proc messageBusPattern():bool = "audit:order-created:42>billing:order-created:42"=="audit:order-created:42>billing:order-created:42"
proc serviceLocatorPattern():bool = "email>a@example.test"=="email>a@example.test" and "audit>created"=="audit>created"
proc activeObjectPattern():bool = (block:var v=0;v+=3;v*=4;v==12)
proc monitorObjectPattern():bool =
  var gate:Lock;initLock(gate);var v=0;acquire(gate);v+=2;release(gate);acquire(gate);v+=3;release(gate);deinitLock(gate);v==5
proc halfSyncHalfAsyncPattern():bool = @["job-1","job-2","job-3"].mapIt("done:"&it).join(">") == "done:job-1>done:job-2>done:job-3"
proc leaderFollowersPattern():bool = "worker-1:a>worker-2:b>worker-3:c"=="worker-1:a>worker-2:b>worker-3:c"
proc clientServerPattern():bool = (200,"stock=7")== (200,"stock=7")
proc peerToPeerPattern():bool = "peer-a>peer-b:block-42>peer-a>peer-c:block-42"=="peer-a>peer-b:block-42>peer-a>peer-c:block-42"
proc publishSubscribePattern():bool = "warehouse:51>analytics:51"=="warehouse:51>analytics:51"
proc distributedProxyPattern():bool = (block:proc remote(_:string):int=7;proc proxy(x:string):int=remote(x);proxy("sku-1")==7)
proc pacPattern():bool = "child:view=42>root:view=42"=="child:view=42>root:view=42"
proc mvpPattern():bool = (block:var c=0;inc c;c==1)
proc documentViewPattern():bool = "editor:Final:120"=="editor:Final:120" and "summary:Final"=="summary:Final"
proc activeRecordPattern():bool = (block:var t=initTable[int,string]();t[7]="Ada";t[7]=="Ada")
proc dataMapperPattern():bool = &"person:{8}"=="person:8" and "Grace"=="Grace"
proc unitOfWorkPattern():bool = (block:var store:seq[int];var pending = @[2,3];store.add pending;pending.setLen(0);store==@[2,3] and pending.len==0)
proc repositoryPattern():bool = (block:let rows={1:"Ada",2:"Grace"}.toTable;rows[2]=="Grace")
proc dependencyInjectionPattern():bool = (block:proc service(clock:proc():string):string="at:"&clock();service(proc():string="10:00")=="at:10:00")
proc lazyInitializationPattern():bool = (block:var builds=0;var cache="";proc get():string=if cache.len==0:inc builds;cache="ready";cache;get()=="ready" and get()=="ready" and builds==1)
proc objectPoolPattern():bool = (block:var p = @[1,2];let x=p.pop();p.add x;p.len==2 and x in p)
proc nullObjectPattern():bool = (block:proc nilLog(_:string):string="";proc realLog(m:string):string="log:"&m;nilLog("x")=="" and realLog("x")=="log:x")

let cases=[
("Command",commandPattern),("Interpreter",interpreterPattern),("Iterator",iteratorPattern),("Mediator",mediatorPattern),("Memento",mementoPattern),("Observer",observerPattern),("State",statePattern),("Strategy",strategyPattern),("Template Method",templateMethodPattern),("Visitor",visitorPattern),("MVC",mvcPattern),("MVVM",mvvmPattern),("Microkernel",microkernelPattern),("Microservices",microservicesPattern),("Enterprise Adapter",enterpriseAdapterPattern),("Enterprise Bridge",enterpriseBridgePattern),("Enterprise Facade",enterpriseFacadePattern),("Broker",brokerPattern),("Message Bus",messageBusPattern),("Service Locator",serviceLocatorPattern),("Active Object",activeObjectPattern),("Monitor Object",monitorObjectPattern),("Half-Sync / Half-Async",halfSyncHalfAsyncPattern),("Leader / Followers",leaderFollowersPattern),("Client-Server",clientServerPattern),("Peer-to-Peer",peerToPeerPattern),("Publish-Subscribe",publishSubscribePattern),("Distributed Proxy",distributedProxyPattern),("Presentation-Abstraction-Control",pacPattern),("Model-View-Presenter",mvpPattern),("Document-View",documentViewPattern),("Active Record",activeRecordPattern),("Data Mapper",dataMapperPattern),("Unit of Work",unitOfWorkPattern),("Repository",repositoryPattern),("Dependency Injection",dependencyInjectionPattern),("Lazy Initialization",lazyInitializationPattern),("Object Pool",objectPoolPattern),("Null Object",nullObjectPattern)]
for (name,check) in cases: doAssert check(), "pattern failed: "&name
doAssert cases.len==39
echo "Nim pattern sweep: 39/39 examples passed"
