import Foundation

func must(_ value: Bool, _ name: String) { precondition(value, "pattern failed: \(name)") }

func commandPattern() -> Bool { let commands:[(Int)->Int]=[{ $0+50 },{ $0-20 }]; let balance=commands.reduce(100){$1($0)}; return balance==130 && commands[1](150)==130 }
indirect enum Expr { case lit(Int), add(Expr,Expr), mul(Expr,Expr) }
func evalExpr(_ e:Expr)->Int { switch e { case .lit(let v): return v; case .add(let l,let r): return evalExpr(l)+evalExpr(r); case .mul(let l,let r): return evalExpr(l)*evalExpr(r) } }
func interpreterPattern()->Bool { evalExpr(.add(.lit(7),.mul(.lit(3),.lit(4))))==19 }
func iteratorPattern()->Bool { var iterator=[10,20,30].makeIterator(); let seen=[iterator.next(),iterator.next(),iterator.next()].compactMap{$0}; return seen==[10,20,30] && iterator.next()==nil }
func mediatorPattern()->Bool { var events:[String]=[]; func notify(_ sender:String,_ event:String){ if sender=="button"&&event=="click"{events.append("panel.refresh")}; if sender=="panel"&&event=="loaded"{events.append("button.enable")} }; notify("button","click");notify("panel","loaded");return events.joined(separator:">") == "panel.refresh>button.enable" }
func mementoPattern()->Bool { var state="draft";let snapshot=state;state="published";guard state=="published" else{return false};state=snapshot;return state=="draft" }
func observerPattern()->Bool { let observers:[(Int)->String]=[{"audit:\($0)"},{"dashboard:\($0)"}]; return observers.map{$0(42)}.joined(separator:">") == "audit:42>dashboard:42" }
enum Gate { case locked,unlocked }; func transition(_ s:Gate,_ a:String)->Gate { if s == .locked && a=="unlock" {return .unlocked}; if s == .unlocked && a=="lock" {return .locked}; return s }
func statePattern()->Bool { transition(transition(.locked,"unlock"),"lock") == .locked }
func strategyPattern()->Bool { let price:(Int,(Int)->Int)->Int={v,s in s(v)};return price(100,{$0})==100&&price(100,{$0*80/100})==80 }
func templateMethodPattern()->Bool { let pipeline:(String,()->String)->String={r,t in "\(r)>\(t())>publish"};return pipeline("read-csv",{"normalize"})=="read-csv>normalize>publish" }
enum Shape {case circle(Double),rect(Double,Double)}; func area(_ s:Shape)->Double {switch s{case .circle(let r):return Double.pi*r*r;case .rect(let w,let h):return w*h}}
func visitorPattern()->Bool { abs([Shape.circle(2),.rect(3,4)].map(area).reduce(0,+)-(4*Double.pi+12))<1e-9 }
func mvcPattern()->Bool { var count=0;let render={"count=\(count)"};let before=render();count+=1;return before=="count=0"&&render()=="count=1" }
func mvvmPattern()->Bool { var amount=10;let text={"$\(amount).00"};let before=text();amount+=5;return before=="$10.00"&&text()=="$15.00" }
func microkernelPattern()->Bool { let plugins:[String:(Int)->Int]=["double":{$0*2},"square":{$0*$0}];return plugins["double"]!(4)==8&&plugins["square"]!(4)==16 }
func microservicesPattern()->Bool { var stock=7;let reserve:(Int)->Bool={q in if q>stock{return false};stock-=q;return true};let place:(Int)->String={reserve($0) ? "confirmed":"rejected"};return place(2)=="confirmed"&&stock==5 }
func enterpriseAdapterPattern()->Bool { let legacy=(code:17,cents:1250);let canonical=(id:legacy.code,amount:Double(legacy.cents)/100);return canonical.id==17&&canonical.amount==12.5 }
func enterpriseBridgePattern()->Bool { let send:(String,String,String)->String={"\($0)>\($1):\($2)"};return send("kafka","ALERT","disk")=="kafka>ALERT:disk"&&send("queue","REMINDER","backup")=="queue>REMINDER:backup" }
func enterpriseFacadePattern()->Bool { let crm:(Int)->String={"crm:create:\($0)"};let billing:(Int)->String={"billing:open:\($0)"};return "\(crm(77))>\(billing(77))"=="crm:create:77>billing:open:77" }
func brokerPattern()->Bool { let services:[String:(String)->String]=["inventory":{"inventory:\($0)=7"},"customer":{"customer:\($0)=active"}];return services["inventory"]!("sku-1")=="inventory:sku-1=7"&&services["customer"]!("17")=="customer:17=active" }
func messageBusPattern()->Bool { let handlers:[(String,Int)->String]=[{"audit:\($0):\($1)"},{"billing:\($0):\($1)"}];return handlers.map{$0("order-created",42)}.joined(separator:">") == "audit:order-created:42>billing:order-created:42" }
func serviceLocatorPattern()->Bool { let s:[String:(String)->String]=["email":{"email>\($0)"},"audit":{"audit>\($0)"}];return s["email"]!("a@example.test")=="email>a@example.test"&&s["audit"]!("created")=="audit>created" }
func activeObjectPattern()->Bool { var value=0;let q:[()->Void]=[{value+=3},{value*=4}];let before=value;q.forEach{$0()};return before==0&&value==12 }
final class MonitorCounter { private let lock=NSLock();private var value=0;func add(_ x:Int){lock.lock();defer{lock.unlock()};value+=x};func get()->Int{lock.lock();defer{lock.unlock()};return value} }
func monitorObjectPattern()->Bool { let c=MonitorCounter();c.add(2);c.add(3);return c.get()==5 }
func halfSyncHalfAsyncPattern()->Bool { ["job-1","job-2","job-3"].map{"done:\($0)"}.joined(separator:">") == "done:job-1>done:job-2>done:job-3" }
func leaderFollowersPattern()->Bool { let w=["worker-1","worker-2","worker-3"],e=["a","b","c"];let h=e.enumerated().map{"\(w[$0.offset%w.count]):\($0.element)"};return h.joined(separator:">") == "worker-1:a>worker-2:b>worker-3:c"&&w[e.count%w.count]=="worker-1" }
func clientServerPattern()->Bool { let server:(String)->(Int,String)={$0=="sku-1" ? (200,"stock=7"):(404,"missing")};let r=server("sku-1");return r.0==200&&r.1=="stock=7" }
func peerToPeerPattern()->Bool { var b:[String]=[],c:[String]=[];func send(_ f:String,_ t:String,_ d:String,_ inbox:inout [String]){inbox.append("\(f)>\(t):\(d)")};send("peer-a","peer-b","block-42",&b);send("peer-a","peer-c","block-42",&c);return (b+c).joined(separator:">") == "peer-a>peer-b:block-42>peer-a>peer-c:block-42" }
func publishSubscribePattern()->Bool { let s:[(Int)->String]=[{"warehouse:\($0)"},{"analytics:\($0)"}];return s.map{$0(51)}.joined(separator:">") == "warehouse:51>analytics:51" }
func distributedProxyPattern()->Bool { let remote:(String)->Int={$0=="sku-1" ? 7:0};let proxy:(String)->Int={remote($0)};return proxy("sku-1")==7 }
func pacPattern()->Bool { let view:(String,Int)->String={"\($0):view=\($1)"};return view("child",42)=="child:view=42"&&view("root",42)=="root:view=42" }
func mvpPattern()->Bool { var count=0,text="";let present={count+=1;text="count=\(count)"};present();return count==1&&text=="count=1" }
func documentViewPattern()->Bool { let d=(title:"Final",words:120);let editor={"editor:\(d.title):\(d.words)"};let summary={"summary:\(d.title)"};return editor()=="editor:Final:120"&&summary()=="summary:Final" }
func activeRecordPattern()->Bool { var table:[Int:String]=[:];table[7]="Ada";return table[7]=="Ada" }
func dataMapperPattern()->Bool { let p=(id:8,name:"Grace");let row=(key:"person:\(p.id)",name:p.name);let restored=(id:Int(row.key.split(separator:":")[1])!,name:row.name);return row.key=="person:8"&&restored.name=="Grace" }
func unitOfWorkPattern()->Bool { var store:[Int]=[],pending=[2,3];store.append(contentsOf:pending);pending.removeAll();return store==[2,3]&&pending.isEmpty }
func repositoryPattern()->Bool { let rows=[(1,"Ada"),(2,"Grace")];return rows.first{$0.0==2}?.1=="Grace" }
func dependencyInjectionPattern()->Bool { let service:(()->String)->String={"at:\($0())"};return service{"10:00"}=="at:10:00" }
func lazyInitializationPattern()->Bool { var builds=0;var value:String?;func get()->String{if value==nil{builds+=1;value="ready"};return value!};let a=get(),b=get();return a=="ready"&&b=="ready"&&builds==1 }
func objectPoolPattern()->Bool { var pool=[1,2];let borrowed=pool.removeLast();pool.append(borrowed);return pool.count==2&&pool.contains(borrowed) }
protocol Logger{func log(_ m:String)->String};struct NullLogger:Logger{func log(_ m:String)->String{""}};struct RealLogger:Logger{func log(_ m:String)->String{"log:\(m)"}}
func nullObjectPattern()->Bool { NullLogger().log("x").isEmpty&&RealLogger().log("x")=="log:x" }

let cases:[(String,()->Bool)]=[("Command",commandPattern),("Interpreter",interpreterPattern),("Iterator",iteratorPattern),("Mediator",mediatorPattern),("Memento",mementoPattern),("Observer",observerPattern),("State",statePattern),("Strategy",strategyPattern),("Template Method",templateMethodPattern),("Visitor",visitorPattern),("MVC",mvcPattern),("MVVM",mvvmPattern),("Microkernel",microkernelPattern),("Microservices",microservicesPattern),("Enterprise Adapter",enterpriseAdapterPattern),("Enterprise Bridge",enterpriseBridgePattern),("Enterprise Facade",enterpriseFacadePattern),("Broker",brokerPattern),("Message Bus",messageBusPattern),("Service Locator",serviceLocatorPattern),("Active Object",activeObjectPattern),("Monitor Object",monitorObjectPattern),("Half-Sync / Half-Async",halfSyncHalfAsyncPattern),("Leader / Followers",leaderFollowersPattern),("Client-Server",clientServerPattern),("Peer-to-Peer",peerToPeerPattern),("Publish-Subscribe",publishSubscribePattern),("Distributed Proxy",distributedProxyPattern),("Presentation-Abstraction-Control",pacPattern),("Model-View-Presenter",mvpPattern),("Document-View",documentViewPattern),("Active Record",activeRecordPattern),("Data Mapper",dataMapperPattern),("Unit of Work",unitOfWorkPattern),("Repository",repositoryPattern),("Dependency Injection",dependencyInjectionPattern),("Lazy Initialization",lazyInitializationPattern),("Object Pool",objectPoolPattern),("Null Object",nullObjectPattern)]
for (name,run) in cases { must(run(),name) }; must(cases.count==39,"count"); print("Swift pattern sweep: 39/39 examples passed")
