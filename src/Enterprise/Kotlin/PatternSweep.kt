import kotlin.math.PI
import kotlin.math.abs

fun must(value: Boolean, name: String) { if (!value) error("pattern failed: $name") }

fun commandPattern(): Boolean { val commands=listOf<(Int)->Int>({it+50},{it-20}); val balance=commands.fold(100){x,c->c(x)}; return balance==130 && commands[1](150)==130 }
sealed interface Expr { data class Lit(val value:Int):Expr; data class Add(val l:Expr,val r:Expr):Expr; data class Mul(val l:Expr,val r:Expr):Expr }
fun evalExpr(e:Expr):Int=when(e){is Expr.Lit->e.value;is Expr.Add->evalExpr(e.l)+evalExpr(e.r);is Expr.Mul->evalExpr(e.l)*evalExpr(e.r)}
fun interpreterPattern()=evalExpr(Expr.Add(Expr.Lit(7),Expr.Mul(Expr.Lit(3),Expr.Lit(4))))==19
fun iteratorPattern():Boolean { val values=listOf(10,20,30); val it=values.iterator(); val seen=mutableListOf<Int>(); while(it.hasNext()) seen+=it.next(); return seen==values&&!it.hasNext() }
fun mediatorPattern():Boolean { val events=mutableListOf<String>(); fun notify(sender:String,event:String){if(sender=="button"&&event=="click")events+="panel.refresh";if(sender=="panel"&&event=="loaded")events+="button.enable"}; notify("button","click");notify("panel","loaded");return events.joinToString(">") == "panel.refresh>button.enable" }
fun mementoPattern():Boolean { var state="draft";val snapshot=state;state="published";if(state!="published")return false;state=snapshot;return state=="draft" }
fun observerPattern():Boolean { val obs=listOf<(Int)->String>({"audit:$it"},{"dashboard:$it"});return obs.map{it(42)}.joinToString(">") == "audit:42>dashboard:42" }
enum class Gate{LOCKED,UNLOCKED}; fun transition(s:Gate,a:String)=if(s==Gate.LOCKED&&a=="unlock")Gate.UNLOCKED else if(s==Gate.UNLOCKED&&a=="lock")Gate.LOCKED else s
fun statePattern()=transition(transition(Gate.LOCKED,"unlock"),"lock")==Gate.LOCKED
fun strategyPattern(): Boolean { val price = { v: Int, s: (Int)->Int -> s(v) }; return price(100){it}==100 && price(100){it*80/100}==80 }
fun templateMethodPattern(): Boolean { val pipeline = { read: String, transform: ()->String -> "$read>${transform()}>publish" }; return pipeline("read-csv") { "normalize" } == "read-csv>normalize>publish" }
sealed interface Shape{data class Circle(val r:Double):Shape;data class Rect(val w:Double,val h:Double):Shape}; fun area(s:Shape)=when(s){is Shape.Circle->PI*s.r*s.r;is Shape.Rect->s.w*s.h}
fun visitorPattern()=abs(listOf<Shape>(Shape.Circle(2.0),Shape.Rect(3.0,4.0)).sumOf(::area)-(4*PI+12))<1e-9
fun mvcPattern(): Boolean { data class Model(var count:Int); val m=Model(0); val render = { "count=${m.count}" }; val before=render(); m.count++; return before=="count=0" && render()=="count=1" }
fun mvvmPattern(): Boolean { data class Vm(var amount:Int); val vm=Vm(10); val text = { "$${vm.amount}.00" }; val before=text(); vm.amount+=5; return before=="$10.00" && text()=="$15.00" }
fun microkernelPattern():Boolean { val plugins=mapOf<String,(Int)->Int>("double" to {it*2},"square" to {it*it});return plugins.getValue("double")(4)==8&&plugins.getValue("square")(4)==16 }
fun microservicesPattern(): Boolean { data class Inventory(var stock:Int); val inv=Inventory(7); val reserve = { q:Int -> if(q>inv.stock) false else { inv.stock-=q; true } }; val place = { q:Int -> if(reserve(q)) "confirmed" else "rejected" }; return place(2)=="confirmed" && inv.stock==5 }
fun enterpriseAdapterPattern():Boolean { data class Legacy(val code:Int,val cents:Int);data class Canonical(val id:Int,val amount:Double);val l=Legacy(17,1250);val c=Canonical(l.code,l.cents/100.0);return c.id==17&&c.amount==12.5 }
fun enterpriseBridgePattern(): Boolean { val send = { t:String,k:String,m:String -> "$t>$k:$m" }; return send("kafka","ALERT","disk")=="kafka>ALERT:disk" && send("queue","REMINDER","backup")=="queue>REMINDER:backup" }
fun enterpriseFacadePattern(): Boolean { val crm = { id:Int -> "crm:create:$id" }; val billing = { id:Int -> "billing:open:$id" }; return "${crm(77)}>${billing(77)}"=="crm:create:77>billing:open:77" }
fun brokerPattern():Boolean { val s=mapOf<String,(String)->String>("inventory" to {"inventory:$it=7"},"customer" to {"customer:$it=active"});return s.getValue("inventory")("sku-1")=="inventory:sku-1=7"&&s.getValue("customer")("17")=="customer:17=active" }
fun messageBusPattern():Boolean { val h=listOf<(String,Int)->String>({t,i->"audit:$t:$i"},{t,i->"billing:$t:$i"});return h.map{it("order-created",42)}.joinToString(">") == "audit:order-created:42>billing:order-created:42" }
fun serviceLocatorPattern():Boolean { val s=mapOf<String,(String)->String>("email" to {"email>$it"},"audit" to {"audit>$it"});return s.getValue("email")("a@example.test")=="email>a@example.test"&&s.getValue("audit")("created")=="audit>created" }
fun activeObjectPattern():Boolean { var v=0;val q=listOf<()->Unit>({v+=3},{v*=4});val before=v;q.forEach{it()};return before==0&&v==12 }
class MonitorCounter{private val lock=Any();private var value=0;fun add(x:Int)=synchronized(lock){value+=x};fun get()=synchronized(lock){value}}
fun monitorObjectPattern():Boolean { val c=MonitorCounter();c.add(2);c.add(3);return c.get()==5 }
fun halfSyncHalfAsyncPattern()=listOf("job-1","job-2","job-3").map{"done:$it"}.joinToString(">") == "done:job-1>done:job-2>done:job-3"
fun leaderFollowersPattern():Boolean { val w=listOf("worker-1","worker-2","worker-3");val e=listOf("a","b","c");val h=e.mapIndexed{i,x->"${w[i%w.size]}:$x"};return h.joinToString(">") == "worker-1:a>worker-2:b>worker-3:c"&&w[e.size%w.size]=="worker-1" }
fun clientServerPattern(): Boolean { data class Resp(val status:Int,val body:String); val server = { k:String -> if(k=="sku-1") Resp(200,"stock=7") else Resp(404,"missing") }; val r=server("sku-1"); return r.status==200 && r.body=="stock=7" }
fun peerToPeerPattern():Boolean { val b=mutableListOf<String>();val c=mutableListOf<String>();fun send(f:String,t:String,d:String,i:MutableList<String>){i+="$f>$t:$d"};send("peer-a","peer-b","block-42",b);send("peer-a","peer-c","block-42",c);return (b+c).joinToString(">") == "peer-a>peer-b:block-42>peer-a>peer-c:block-42" }
fun publishSubscribePattern():Boolean { val s=listOf<(Int)->String>({"warehouse:$it"},{"analytics:$it"});return s.map{it(51)}.joinToString(">") == "warehouse:51>analytics:51" }
fun distributedProxyPattern(): Boolean { val remote = { sku:String -> if(sku=="sku-1") 7 else 0 }; val proxy = { sku:String -> remote(sku) }; return proxy("sku-1")==7 }
fun pacPattern(): Boolean { val view = { n:String,v:Int -> "$n:view=$v" }; return view("child",42)=="child:view=42" && view("root",42)=="root:view=42" }
fun mvpPattern():Boolean { data class Model(var count:Int);data class View(var text:String);val m=Model(0);val v=View("");fun present(){m.count++;v.text="count=${m.count}"};present();return m.count==1&&v.text=="count=1" }
fun documentViewPattern(): Boolean { data class Doc(val title:String,val words:Int); val d=Doc("Final",120); val editor={"editor:${d.title}:${d.words}"}; val summary={"summary:${d.title}"}; return editor()=="editor:Final:120" && summary()=="summary:Final" }
fun activeRecordPattern():Boolean { data class P(val id:Int,val name:String);val table=mutableMapOf<Int,P>();val p=P(7,"Ada");table[p.id]=p;return table[7]?.name=="Ada" }
fun dataMapperPattern():Boolean { data class P(val id:Int,val name:String);data class R(val key:String,val name:String);val p=P(8,"Grace");val r=R("person:${p.id}",p.name);val restored=P(r.key.substringAfter(':').toInt(),r.name);return r.key=="person:8"&&restored.name=="Grace" }
fun unitOfWorkPattern():Boolean { val store=mutableListOf<Int>();val pending=mutableListOf(2,3);store+=pending;pending.clear();return store==listOf(2,3)&&pending.isEmpty() }
fun repositoryPattern():Boolean { data class P(val id:Int,val name:String);val rows=listOf(P(1,"Ada"),P(2,"Grace"));return rows.find{it.id==2}?.name=="Grace" }
fun dependencyInjectionPattern(): Boolean { val service = { clock:()->String -> "at:${clock()}" }; return service{"10:00"}=="at:10:00" }
fun lazyInitializationPattern():Boolean { var builds=0;val value by lazy{builds++;"ready"};val a=value;val b=value;return a=="ready"&&b=="ready"&&builds==1 }
fun objectPoolPattern():Boolean { data class Item(val id:Int);val pool=mutableListOf(Item(1),Item(2));val borrowed=pool.removeLast();pool+=borrowed;return pool.size==2&&pool.any{it.id==borrowed.id} }
interface Logger{fun log(m:String):String}; object NullLogger:Logger{override fun log(m:String)=""}; object RealLogger:Logger{override fun log(m:String)="log:$m"}
fun nullObjectPattern()=NullLogger.log("x")==""&&RealLogger.log("x")=="log:x"

fun main(){
 val cases=listOf<Pair<String,()->Boolean>>(
  "Command" to ::commandPattern,"Interpreter" to ::interpreterPattern,"Iterator" to ::iteratorPattern,"Mediator" to ::mediatorPattern,"Memento" to ::mementoPattern,"Observer" to ::observerPattern,"State" to ::statePattern,"Strategy" to ::strategyPattern,"Template Method" to ::templateMethodPattern,"Visitor" to ::visitorPattern,"MVC" to ::mvcPattern,"MVVM" to ::mvvmPattern,"Microkernel" to ::microkernelPattern,"Microservices" to ::microservicesPattern,"Enterprise Adapter" to ::enterpriseAdapterPattern,"Enterprise Bridge" to ::enterpriseBridgePattern,"Enterprise Facade" to ::enterpriseFacadePattern,"Broker" to ::brokerPattern,"Message Bus" to ::messageBusPattern,"Service Locator" to ::serviceLocatorPattern,"Active Object" to ::activeObjectPattern,"Monitor Object" to ::monitorObjectPattern,"Half-Sync / Half-Async" to ::halfSyncHalfAsyncPattern,"Leader / Followers" to ::leaderFollowersPattern,"Client-Server" to ::clientServerPattern,"Peer-to-Peer" to ::peerToPeerPattern,"Publish-Subscribe" to ::publishSubscribePattern,"Distributed Proxy" to ::distributedProxyPattern,"Presentation-Abstraction-Control" to ::pacPattern,"Model-View-Presenter" to ::mvpPattern,"Document-View" to ::documentViewPattern,"Active Record" to ::activeRecordPattern,"Data Mapper" to ::dataMapperPattern,"Unit of Work" to ::unitOfWorkPattern,"Repository" to ::repositoryPattern,"Dependency Injection" to ::dependencyInjectionPattern,"Lazy Initialization" to ::lazyInitializationPattern,"Object Pool" to ::objectPoolPattern,"Null Object" to ::nullObjectPattern)
 cases.forEach{(n,f)->must(f(),n)}; must(cases.size==39,"count"); println("Kotlin pattern sweep: 39/39 examples passed")
}
