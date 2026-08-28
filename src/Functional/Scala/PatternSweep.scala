import scala.collection.mutable

object PatternSweep {
  def commandPattern: Boolean = { val cs=List[Int=>Int](_+50,_-20); val b=cs.foldLeft(100)((x,f)=>f(x)); b==130 && cs(1)(150)==130 }
  sealed trait Expr; case class Lit(v:Int) extends Expr; case class Add(l:Expr,r:Expr) extends Expr; case class Mul(l:Expr,r:Expr) extends Expr
  def eval(e:Expr):Int=e match{case Lit(v)=>v;case Add(l,r)=>eval(l)+eval(r);case Mul(l,r)=>eval(l)*eval(r)}
  def interpreterPattern:Boolean=eval(Add(Lit(7),Mul(Lit(3),Lit(4))))==19
  def iteratorPattern:Boolean={val it=List(10,20,30).iterator;val seen=it.toList;seen==List(10,20,30)&&!it.hasNext}
  def mediatorPattern:Boolean={val e=mutable.ArrayBuffer[String]();def notify(s:String,x:String):Unit={if(s=="button"&&x=="click")e+="panel.refresh";if(s=="panel"&&x=="loaded")e+="button.enable"};notify("button","click");notify("panel","loaded");e.mkString(">") == "panel.refresh>button.enable"}
  def mementoPattern:Boolean={var s="draft";val snap=s;s="published";s=snap;s=="draft"}
  def observerPattern:Boolean=List[Int=>String](i=>s"audit:$i",i=>s"dashboard:$i").map(_(42)).mkString(">")=="audit:42>dashboard:42"
  def statePattern:Boolean={def t(s:String,a:String)=if(s=="locked"&&a=="unlock")"unlocked"else if(s=="unlocked"&&a=="lock")"locked"else s;t(t("locked","unlock"),"lock")=="locked"}
  def strategyPattern:Boolean={def price(v:Int,s:Int=>Int)=s(v);price(100,identity)==100&&price(100,_*80/100)==80}
  def templateMethodPattern:Boolean={def pipe(r:String,t:()=>String)=s"$r>${t()}>publish";pipe("read-csv",()=>"normalize")=="read-csv>normalize>publish"}
  sealed trait Shape;case class Circle(r:Double)extends Shape;case class Rect(w:Double,h:Double)extends Shape;def area(s:Shape):Double=s match{case Circle(r)=>math.Pi*r*r;case Rect(w,h)=>w*h}
  def visitorPattern:Boolean=math.abs(List[Shape](Circle(2),Rect(3,4)).map(area).sum-(4*math.Pi+12))<1e-9
  def mvcPattern:Boolean={var c=0;def view=s"count=$c";val before=view;c+=1;before=="count=0"&&view=="count=1"}
  def mvvmPattern:Boolean={var a=10;def text=s"$$$a.00";val before=text;a+=5;before=="$10.00"&&text=="$15.00"}
  def microkernelPattern:Boolean={val p=Map[String,Int=>Int]("double"->_*2,"square"->(x=>x*x));p("double")(4)==8&&p("square")(4)==16}
  def microservicesPattern:Boolean={var stock=7;def reserve(q:Int)=if(q>stock)false else{stock-=q;true};def place(q:Int)=if(reserve(q))"confirmed"else"rejected";place(2)=="confirmed"&&stock==5}
  def enterpriseAdapterPattern:Boolean={val legacy=(17,1250);val canonical=(legacy._1,legacy._2/100.0);canonical==(17,12.5)}
  def enterpriseBridgePattern:Boolean={def send(t:String,k:String,m:String)=s"$t>$k:$m";send("kafka","ALERT","disk")=="kafka>ALERT:disk"&&send("queue","REMINDER","backup")=="queue>REMINDER:backup"}
  def enterpriseFacadePattern:Boolean={def crm(i:Int)=s"crm:create:$i";def billing(i:Int)=s"billing:open:$i";s"${crm(77)}>${billing(77)}"=="crm:create:77>billing:open:77"}
  def brokerPattern:Boolean={val s=Map[String,String=>String]("inventory"->(k=>s"inventory:$k=7"),"customer"->(k=>s"customer:$k=active"));s("inventory")("sku-1")=="inventory:sku-1=7"&&s("customer")("17")=="customer:17=active"}
  def messageBusPattern:Boolean=List[(String,Int)=>String]((t,i)=>s"audit:$t:$i",(t,i)=>s"billing:$t:$i").map(_("order-created",42)).mkString(">") == "audit:order-created:42>billing:order-created:42"
  def serviceLocatorPattern:Boolean={val s=Map[String,String=>String]("email"->(v=>s"email>$v"),"audit"->(v=>s"audit>$v"));s("email")("a@example.test")=="email>a@example.test"&&s("audit")("created")=="audit>created"}
  def activeObjectPattern:Boolean={var v=0;val q=List[()=>Unit](()=>v+=3,()=>v*=4);val before=v;q.foreach(_());before==0&&v==12}
  def monitorObjectPattern:Boolean={class C{private var v=0;def add(x:Int):Unit=this.synchronized{v+=x};def get:Int=this.synchronized{v}};val c=new C;c.add(2);c.add(3);c.get==5}
  def halfSyncHalfAsyncPattern:Boolean=List("job-1","job-2","job-3").map("done:"+_).mkString(">") == "done:job-1>done:job-2>done:job-3"
  def leaderFollowersPattern:Boolean={val w=Vector("worker-1","worker-2","worker-3");val e=Vector("a","b","c");e.indices.map(i=>s"${w(i%w.size)}:${e(i)}").mkString(">") == "worker-1:a>worker-2:b>worker-3:c"&&w(e.size%w.size)=="worker-1"}
  def clientServerPattern:Boolean={def server(k:String)=if(k=="sku-1")(200,"stock=7")else(404,"missing");server("sku-1")==((200,"stock=7"))}
  def peerToPeerPattern:Boolean={val b=mutable.ArrayBuffer[String]();val c=mutable.ArrayBuffer[String]();def send(f:String,t:String,d:String,i:mutable.ArrayBuffer[String])=i+=s"$f>$t:$d";send("peer-a","peer-b","block-42",b);send("peer-a","peer-c","block-42",c);(b++c).mkString(">") == "peer-a>peer-b:block-42>peer-a>peer-c:block-42"}
  def publishSubscribePattern:Boolean=List[Int=>String](i=>s"warehouse:$i",i=>s"analytics:$i").map(_(51)).mkString(">") == "warehouse:51>analytics:51"
  def distributedProxyPattern:Boolean={def remote(s:String)=if(s=="sku-1")7 else 0;def proxy(s:String)=remote(s);proxy("sku-1")==7}
  def pacPattern:Boolean={def view(n:String,v:Int)=s"$n:view=$v";view("child",42)=="child:view=42"&&view("root",42)=="root:view=42"}
  def mvpPattern:Boolean={var count=0;var text="";def present():Unit={count+=1;text=s"count=$count"};present();count==1&&text=="count=1"}
  def documentViewPattern:Boolean={val d=("Final",120);s"editor:${d._1}:${d._2}"=="editor:Final:120"&&s"summary:${d._1}"=="summary:Final"}
  def activeRecordPattern:Boolean={val t=mutable.Map[Int,String]();t(7)="Ada";t(7)=="Ada"}
  def dataMapperPattern:Boolean={val p=(8,"Grace");val row=(s"person:${p._1}",p._2);row._1=="person:8"&&row._2=="Grace"}
  def unitOfWorkPattern:Boolean={val store=mutable.ArrayBuffer[Int]();val pending=mutable.ArrayBuffer(2,3);store++=pending;pending.clear();store.toList==List(2,3)&&pending.isEmpty}
  def repositoryPattern:Boolean=List(1->"Ada",2->"Grace").find(_._1==2).exists(_._2=="Grace")
  def dependencyInjectionPattern:Boolean={def service(clock:()=>String)=s"at:${clock()}";service(()=>"10:00")=="at:10:00"}
  def lazyInitializationPattern:Boolean={var builds=0;lazy val value={builds+=1;"ready"};val a=value;val b=value;a=="ready"&&b=="ready"&&builds==1}
  def objectPoolPattern:Boolean={val pool=mutable.Stack(1,2);val x=pool.pop();pool.push(x);pool.size==2&&pool.contains(x)}
  trait Logger{def log(m:String):String};object NullLogger extends Logger{def log(m:String)=""};object RealLogger extends Logger{def log(m:String)=s"log:$m"}
  def nullObjectPattern:Boolean=NullLogger.log("x").isEmpty&&RealLogger.log("x")=="log:x"
  val cases=List[(String,()=>Boolean)](("Command",()=>commandPattern),("Interpreter",()=>interpreterPattern),("Iterator",()=>iteratorPattern),("Mediator",()=>mediatorPattern),("Memento",()=>mementoPattern),("Observer",()=>observerPattern),("State",()=>statePattern),("Strategy",()=>strategyPattern),("Template Method",()=>templateMethodPattern),("Visitor",()=>visitorPattern),("MVC",()=>mvcPattern),("MVVM",()=>mvvmPattern),("Microkernel",()=>microkernelPattern),("Microservices",()=>microservicesPattern),("Enterprise Adapter",()=>enterpriseAdapterPattern),("Enterprise Bridge",()=>enterpriseBridgePattern),("Enterprise Facade",()=>enterpriseFacadePattern),("Broker",()=>brokerPattern),("Message Bus",()=>messageBusPattern),("Service Locator",()=>serviceLocatorPattern),("Active Object",()=>activeObjectPattern),("Monitor Object",()=>monitorObjectPattern),("Half-Sync / Half-Async",()=>halfSyncHalfAsyncPattern),("Leader / Followers",()=>leaderFollowersPattern),("Client-Server",()=>clientServerPattern),("Peer-to-Peer",()=>peerToPeerPattern),("Publish-Subscribe",()=>publishSubscribePattern),("Distributed Proxy",()=>distributedProxyPattern),("Presentation-Abstraction-Control",()=>pacPattern),("Model-View-Presenter",()=>mvpPattern),("Document-View",()=>documentViewPattern),("Active Record",()=>activeRecordPattern),("Data Mapper",()=>dataMapperPattern),("Unit of Work",()=>unitOfWorkPattern),("Repository",()=>repositoryPattern),("Dependency Injection",()=>dependencyInjectionPattern),("Lazy Initialization",()=>lazyInitializationPattern),("Object Pool",()=>objectPoolPattern),("Null Object",()=>nullObjectPattern))
  def main(args:Array[String]):Unit={cases.foreach{case(n,f)=>require(f(),s"pattern failed: $n")};require(cases.size==39);println("Scala pattern sweep: 39/39 examples passed")}
}
