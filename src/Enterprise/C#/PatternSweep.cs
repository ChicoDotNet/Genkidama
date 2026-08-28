using System;
using System.Collections.Generic;
using System.Linq;

public static class PatternSweep
{
    private static bool CommandPattern(){Func<int,int>[] q={x=>x+50,x=>x-20};var b=q.Aggregate(100,(v,f)=>f(v));return b==130&&q[1](150)==130;}
    private abstract record Expr; private sealed record Lit(int V):Expr; private sealed record Add(Expr L,Expr R):Expr; private sealed record Mul(Expr L,Expr R):Expr;
    private static int Eval(Expr e)=>e switch{Lit x=>x.V,Add x=>Eval(x.L)+Eval(x.R),Mul x=>Eval(x.L)*Eval(x.R),_=>throw new InvalidOperationException()};
    private static bool InterpreterPattern()=>Eval(new Add(new Lit(7),new Mul(new Lit(3),new Lit(4))))==19;
    private static bool IteratorPattern(){using var it=new List<int>{10,20,30}.GetEnumerator();var seen=new List<int>();while(it.MoveNext())seen.Add(it.Current);return seen.SequenceEqual(new[]{10,20,30})&&!it.MoveNext();}
    private static bool MediatorPattern(){var e=new List<string>();void Notify(string s,string x){if(s=="button"&&x=="click")e.Add("panel.refresh");if(s=="panel"&&x=="loaded")e.Add("button.enable");}Notify("button","click");Notify("panel","loaded");return string.Join('>',e)=="panel.refresh>button.enable";}
    private static bool MementoPattern(){var state="draft";var snapshot=state;state="published";state=snapshot;return state=="draft";}
    private static bool ObserverPattern(){Func<int,string>[] obs={i=>$"audit:{i}",i=>$"dashboard:{i}"};return string.Join('>',obs.Select(o=>o(42)))=="audit:42>dashboard:42";}
    private static bool StatePattern(){string T(string s,string a)=>s=="locked"&&a=="unlock"?"unlocked":s=="unlocked"&&a=="lock"?"locked":s;return T(T("locked","unlock"),"lock")=="locked";}
    private static bool StrategyPattern(){int Price(int v,Func<int,int>s)=>s(v);return Price(100,x=>x)==100&&Price(100,x=>x*80/100)==80;}
    private static bool TemplateMethodPattern(){string Pipe(string r,Func<string>t)=>$"{r}>{t()}>publish";return Pipe("read-csv",()=>"normalize")=="read-csv>normalize>publish";}
    private interface IShape{} private sealed record Circle(double R):IShape;private sealed record Rect(double W,double H):IShape;private static double Area(IShape s)=>s switch{Circle c=>Math.PI*c.R*c.R,Rect r=>r.W*r.H,_=>0};
    private static bool VisitorPattern()=>Math.Abs(new IShape[]{new Circle(2),new Rect(3,4)}.Sum(Area)-(4*Math.PI+12))<1e-9;
    private static bool MvcPattern(){var count=0;string View()=>$"count={count}";var before=View();count++;return before=="count=0"&&View()=="count=1";}
    private static bool MvvmPattern(){var amount=10;string Text()=>$"${amount}.00";var before=Text();amount+=5;return before=="$10.00"&&Text()=="$15.00";}
    private static bool MicrokernelPattern(){var p=new Dictionary<string,Func<int,int>>{{"double",x=>x*2},{"square",x=>x*x}};return p["double"](4)==8&&p["square"](4)==16;}
    private static bool MicroservicesPattern(){var stock=7;bool Reserve(int q){if(q>stock)return false;stock-=q;return true;}string Place(int q)=>Reserve(q)?"confirmed":"rejected";return Place(2)=="confirmed"&&stock==5;}
    private static bool EnterpriseAdapterPattern(){var legacy=(Code:17,Cents:1250);var canonical=(Id:legacy.Code,Amount:legacy.Cents/100.0);return canonical==(17,12.5);}
    private static bool EnterpriseBridgePattern(){string Send(string t,string k,string m)=>$"{t}>{k}:{m}";return Send("kafka","ALERT","disk")=="kafka>ALERT:disk"&&Send("queue","REMINDER","backup")=="queue>REMINDER:backup";}
    private static bool EnterpriseFacadePattern(){string Crm(int i)=>$"crm:create:{i}";string Billing(int i)=>$"billing:open:{i}";return $"{Crm(77)}>{Billing(77)}"=="crm:create:77>billing:open:77";}
    private static bool BrokerPattern(){var s=new Dictionary<string,Func<string,string>>{{"inventory",k=>$"inventory:{k}=7"},{"customer",k=>$"customer:{k}=active"}};return s["inventory"]("sku-1")=="inventory:sku-1=7"&&s["customer"]("17")=="customer:17=active";}
    private static bool MessageBusPattern(){Func<string,int,string>[] h={(t,i)=>$"audit:{t}:{i}",(t,i)=>$"billing:{t}:{i}"};return string.Join('>',h.Select(x=>x("order-created",42)))=="audit:order-created:42>billing:order-created:42";}
    private static bool ServiceLocatorPattern(){var s=new Dictionary<string,Func<string,string>>{{"email",v=>$"email>{v}"},{"audit",v=>$"audit>{v}"}};return s["email"]("a@example.test")=="email>a@example.test"&&s["audit"]("created")=="audit>created";}
    private static bool ActiveObjectPattern(){var v=0;Action[] q={()=>v+=3,()=>v*=4};var before=v;foreach(var a in q)a();return before==0&&v==12;}
    private sealed class MonitoredCounter{private readonly object _gate=new();private int _value;public void Add(int x){lock(_gate)_value+=x;}public int Value{get{lock(_gate)return _value;}}}
    private static bool MonitorObjectPattern(){var c=new MonitoredCounter();c.Add(2);c.Add(3);return c.Value==5;}
    private static bool HalfSyncHalfAsyncPattern()=>string.Join('>',new[]{"job-1","job-2","job-3"}.Select(j=>$"done:{j}"))=="done:job-1>done:job-2>done:job-3";
    private static bool LeaderFollowersPattern(){var w=new[]{"worker-1","worker-2","worker-3"};var e=new[]{"a","b","c"};var handled=e.Select((x,i)=>$"{w[i%w.Length]}:{x}");return string.Join('>',handled)=="worker-1:a>worker-2:b>worker-3:c"&&w[e.Length%w.Length]=="worker-1";}
    private static bool ClientServerPattern(){(int,string) Server(string k)=>k=="sku-1"?(200,"stock=7"):(404,"missing");return Server("sku-1")==((200,"stock=7"));}
    private static bool PeerToPeerPattern(){var inbox=new List<string>();void Send(string f,string t,string d)=>inbox.Add($"{f}>{t}:{d}");Send("peer-a","peer-b","block-42");Send("peer-a","peer-c","block-42");return string.Join('>',inbox)=="peer-a>peer-b:block-42>peer-a>peer-c:block-42";}
    private static bool PublishSubscribePattern(){Func<int,string>[] s={i=>$"warehouse:{i}",i=>$"analytics:{i}"};return string.Join('>',s.Select(x=>x(51)))=="warehouse:51>analytics:51";}
    private static bool DistributedProxyPattern(){int Remote(string sku)=>sku=="sku-1"?7:0;int Proxy(string sku)=>Remote(sku);return Proxy("sku-1")==7;}
    private static bool PacPattern(){string View(string n,int v)=>$"{n}:view={v}";return View("child",42)=="child:view=42"&&View("root",42)=="root:view=42";}
    private static bool MvpPattern(){var count=0;var text="";void Present(){count++;text=$"count={count}";}Present();return count==1&&text=="count=1";}
    private static bool DocumentViewPattern(){var d=(Title:"Final",Words:120);return $"editor:{d.Title}:{d.Words}"=="editor:Final:120"&&$"summary:{d.Title}"=="summary:Final";}
    private static bool ActiveRecordPattern(){var t=new Dictionary<int,string>{{7,"Ada"}};return t[7]=="Ada";}
    private static bool DataMapperPattern(){var p=(Id:8,Name:"Grace");var row=(Key:$"person:{p.Id}",p.Name);return row.Key=="person:8"&&row.Name=="Grace";}
    private static bool UnitOfWorkPattern(){var store=new List<int>();var pending=new List<int>{2,3};store.AddRange(pending);pending.Clear();return store.SequenceEqual(new[]{2,3})&&pending.Count==0;}
    private static bool RepositoryPattern()=>new[]{(Id:1,Name:"Ada"),(Id:2,Name:"Grace")}.Single(x=>x.Id==2).Name=="Grace";
    private static bool DependencyInjectionPattern(){string Service(Func<string>clock)=>$"at:{clock()}";return Service(()=>"10:00")=="at:10:00";}
    private static bool LazyInitializationPattern(){var builds=0;string? value=null;string Get()=>value??=Build();string Build(){builds++;return "ready";}return Get()=="ready"&&Get()=="ready"&&builds==1;}
    private static bool ObjectPoolPattern(){var pool=new Stack<int>(new[]{1,2});var x=pool.Pop();pool.Push(x);return pool.Count==2&&pool.Contains(x);}
    private interface ILogger{string Log(string m);}private sealed class NullLogger:ILogger{public string Log(string m)=>"";}private sealed class RealLogger:ILogger{public string Log(string m)=>$"log:{m}";}
    private static bool NullObjectPattern()=>new NullLogger().Log("x")==""&&new RealLogger().Log("x")=="log:x";

    public static void Main()
    {
        (string Name,Func<bool> Check)[] cases={
            ("Command",CommandPattern),("Interpreter",InterpreterPattern),("Iterator",IteratorPattern),("Mediator",MediatorPattern),("Memento",MementoPattern),("Observer",ObserverPattern),("State",StatePattern),("Strategy",StrategyPattern),("Template Method",TemplateMethodPattern),("Visitor",VisitorPattern),("MVC",MvcPattern),("MVVM",MvvmPattern),("Microkernel",MicrokernelPattern),("Microservices",MicroservicesPattern),("Enterprise Adapter",EnterpriseAdapterPattern),("Enterprise Bridge",EnterpriseBridgePattern),("Enterprise Facade",EnterpriseFacadePattern),("Broker",BrokerPattern),("Message Bus",MessageBusPattern),("Service Locator",ServiceLocatorPattern),("Active Object",ActiveObjectPattern),("Monitor Object",MonitorObjectPattern),("Half-Sync / Half-Async",HalfSyncHalfAsyncPattern),("Leader / Followers",LeaderFollowersPattern),("Client-Server",ClientServerPattern),("Peer-to-Peer",PeerToPeerPattern),("Publish-Subscribe",PublishSubscribePattern),("Distributed Proxy",DistributedProxyPattern),("Presentation-Abstraction-Control",PacPattern),("Model-View-Presenter",MvpPattern),("Document-View",DocumentViewPattern),("Active Record",ActiveRecordPattern),("Data Mapper",DataMapperPattern),("Unit of Work",UnitOfWorkPattern),("Repository",RepositoryPattern),("Dependency Injection",DependencyInjectionPattern),("Lazy Initialization",LazyInitializationPattern),("Object Pool",ObjectPoolPattern),("Null Object",NullObjectPattern)};
        foreach(var (name,check) in cases) if(!check()) throw new InvalidOperationException($"pattern failed: {name}");
        if(cases.Length!=39) throw new InvalidOperationException($"expected 39 cases, got {cases.Length}");
        Console.WriteLine("C# pattern sweep: 39/39 examples passed");
    }
}
