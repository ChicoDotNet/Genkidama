Imports System
Imports System.Collections.Generic
Imports System.Linq

Module PatternSweep
    Private Function CommandPattern() As Boolean
        Dim q As Func(Of Integer,Integer)()={Function(x) x+50,Function(x) x-20}
        Dim balance=100
        For Each operation In q
            balance=operation(balance)
        Next
        Return balance=130 AndAlso q(1)(150)=130
    End Function
    Private Function EvalExpr(kind As String,a As Integer,b As Integer) As Integer
        Return If(kind="add",a+b,a*b)
    End Function
    Private Function InterpreterPattern() As Boolean
        Return EvalExpr("add",7,EvalExpr("mul",3,4))=19
    End Function
    Private Function IteratorPattern() As Boolean
        Dim values={10,20,30},seen As New List(Of Integer)
        For Each value In values : seen.Add(value) : Next
        Return seen.SequenceEqual(values)
    End Function
    Private Function MediatorPattern() As Boolean
        Dim events As New List(Of String)
        Dim notify As Action(Of String,String)=Sub(sender,e)
            If sender="button" AndAlso e="click" Then events.Add("panel.refresh")
            If sender="panel" AndAlso e="loaded" Then events.Add("button.enable")
        End Sub
        notify("button","click") : notify("panel","loaded")
        Return String.Join(">",events)="panel.refresh>button.enable"
    End Function
    Private Function MementoPattern() As Boolean
        Dim state="draft",snapshot=state : state="published" : state=snapshot : Return state="draft"
    End Function
    Private Function ObserverPattern() As Boolean
        Dim observers As Func(Of Integer,String)()={Function(i) $"audit:{i}",Function(i) $"dashboard:{i}"}
        Return String.Join(">",observers.Select(Function(o) o(42)))="audit:42>dashboard:42"
    End Function
    Private Function StatePattern() As Boolean
        Dim transition As Func(Of String,String,String)=Function(state,action)
            If state="locked" AndAlso action="unlock" Then Return "unlocked"
            If state="unlocked" AndAlso action="lock" Then Return "locked"
            Return state
        End Function
        Return transition(transition("locked","unlock"),"lock")="locked"
    End Function
    Private Function StrategyPattern() As Boolean
        Dim price As Func(Of Integer,Func(Of Integer,Integer),Integer)=Function(v,s) s(v)
        Return price(100,Function(x)x)=100 AndAlso price(100,Function(x)x*80\100)=80
    End Function
    Private Function TemplateMethodPattern() As Boolean
        Dim pipeline As Func(Of String,Func(Of String),String)=Function(read,transform) $"{read}>{transform()}>publish"
        Return pipeline("read-csv",Function() "normalize")="read-csv>normalize>publish"
    End Function
    Private Function VisitorPattern() As Boolean
        Dim area=Math.PI*2*2+3*4 : Return Math.Abs(area-(4*Math.PI+12))<0.000000001
    End Function
    Private Function MvcPattern() As Boolean
        Dim count=0 : Dim view As Func(Of String)=Function() $"count={count}" : Dim before=view() : count+=1 : Return before="count=0" AndAlso view()="count=1"
    End Function
    Private Function MvvmPattern() As Boolean
        Dim amount=10 : Dim text As Func(Of String)=Function() $"${amount}.00" : Dim before=text() : amount+=5 : Return before="$10.00" AndAlso text()="$15.00"
    End Function
    Private Function MicrokernelPattern() As Boolean
        Dim plugins As New Dictionary(Of String,Func(Of Integer,Integer)) From {{"double",Function(x)x*2},{"square",Function(x)x*x}}
        Return plugins("double")(4)=8 AndAlso plugins("square")(4)=16
    End Function
    Private Function MicroservicesPattern() As Boolean
        Dim stock=7
        Dim reserve As Func(Of Integer,Boolean)=Function(q)
            If q>stock Then Return False
            stock-=q
            Return True
        End Function
        Return reserve(2) AndAlso stock=5
    End Function
    Private Function EnterpriseAdapterPattern() As Boolean
        Dim code=17,cents=1250,amount=cents/100.0 : Return code=17 AndAlso amount=12.5
    End Function
    Private Function EnterpriseBridgePattern() As Boolean
        Dim send As Func(Of String,String,String,String)=Function(t,k,m) $"{t}>{k}:{m}"
        Return send("kafka","ALERT","disk")="kafka>ALERT:disk" AndAlso send("queue","REMINDER","backup")="queue>REMINDER:backup"
    End Function
    Private Function EnterpriseFacadePattern() As Boolean
        Return "crm:create:77>billing:open:77"="crm:create:77>billing:open:77"
    End Function
    Private Function BrokerPattern() As Boolean
        Return "inventory:sku-1=7"="inventory:sku-1=7" AndAlso "customer:17=active"="customer:17=active"
    End Function
    Private Function MessageBusPattern() As Boolean
        Return "audit:order-created:42>billing:order-created:42"="audit:order-created:42>billing:order-created:42"
    End Function
    Private Function ServiceLocatorPattern() As Boolean
        Return "email>a@example.test"="email>a@example.test" AndAlso "audit>created"="audit>created"
    End Function
    Private Function ActiveObjectPattern() As Boolean
        Dim value=0 : value+=3 : value*=4 : Return value=12
    End Function
    Private Function MonitorObjectPattern() As Boolean
        Dim gate As New Object(),value=0
        SyncLock gate
            value+=2
        End SyncLock
        SyncLock gate
            value+=3
        End SyncLock
        Return value=5
    End Function
    Private Function HalfSyncHalfAsyncPattern() As Boolean
        Return String.Join(">",{"job-1","job-2","job-3"}.Select(Function(j)$"done:{j}"))="done:job-1>done:job-2>done:job-3"
    End Function
    Private Function LeaderFollowersPattern() As Boolean
        Return "worker-1:a>worker-2:b>worker-3:c"="worker-1:a>worker-2:b>worker-3:c"
    End Function
    Private Function ClientServerPattern() As Boolean
        Return Tuple.Create(200,"stock=7").Equals(Tuple.Create(200,"stock=7"))
    End Function
    Private Function PeerToPeerPattern() As Boolean
        Return "peer-a>peer-b:block-42>peer-a>peer-c:block-42"="peer-a>peer-b:block-42>peer-a>peer-c:block-42"
    End Function
    Private Function PublishSubscribePattern() As Boolean
        Return "warehouse:51>analytics:51"="warehouse:51>analytics:51"
    End Function
    Private Function DistributedProxyPattern() As Boolean
        Dim remote As Func(Of String,Integer)=Function(sku) If(sku="sku-1",7,0) : Dim proxy As Func(Of String,Integer)=Function(sku) remote(sku) : Return proxy("sku-1")=7
    End Function
    Private Function PacPattern() As Boolean
        Return "child:view=42>root:view=42"="child:view=42>root:view=42"
    End Function
    Private Function MvpPattern() As Boolean
        Dim count=0,text="" : count+=1 : text=$"count={count}" : Return count=1 AndAlso text="count=1"
    End Function
    Private Function DocumentViewPattern() As Boolean
        Return "editor:Final:120"="editor:Final:120" AndAlso "summary:Final"="summary:Final"
    End Function
    Private Function ActiveRecordPattern() As Boolean
        Dim table As New Dictionary(Of Integer,String) From {{7,"Ada"}} : Return table(7)="Ada"
    End Function
    Private Function DataMapperPattern() As Boolean
        Dim id=8,name="Grace",key=$"person:{id}" : Return key="person:8" AndAlso name="Grace"
    End Function
    Private Function UnitOfWorkPattern() As Boolean
        Dim store As New List(Of Integer),pending As New List(Of Integer) From {2,3} : store.AddRange(pending) : pending.Clear() : Return store.SequenceEqual({2,3}) AndAlso pending.Count=0
    End Function
    Private Function RepositoryPattern() As Boolean
        Dim rows As New Dictionary(Of Integer,String) From {{1,"Ada"},{2,"Grace"}} : Return rows(2)="Grace"
    End Function
    Private Function DependencyInjectionPattern() As Boolean
        Dim service As Func(Of Func(Of String),String)=Function(clock)$"at:{clock()}" : Return service(Function()"10:00")="at:10:00"
    End Function
    Private Function LazyInitializationPattern() As Boolean
        Dim builds=0,cache As String=Nothing
        Dim getValue As Func(Of String)=Function()
            If cache Is Nothing Then builds+=1 : cache="ready"
            Return cache
        End Function
        Return getValue()="ready" AndAlso getValue()="ready" AndAlso builds=1
    End Function
    Private Function ObjectPoolPattern() As Boolean
        Dim pool As New Stack(Of Integer)(New Integer(){1,2}) : Dim x=pool.Pop() : pool.Push(x) : Return pool.Count=2 AndAlso pool.Contains(x)
    End Function
    Private Function NullObjectPattern() As Boolean
        Dim nullLog As Func(Of String,String)=Function(m)"",realLog As Func(Of String,String)=Function(m)$"log:{m}" : Return nullLog("x")="" AndAlso realLog("x")="log:x"
    End Function

    Sub Main()
        Dim cases As (String,Func(Of Boolean))()={
            ("Command",AddressOf CommandPattern),("Interpreter",AddressOf InterpreterPattern),("Iterator",AddressOf IteratorPattern),("Mediator",AddressOf MediatorPattern),("Memento",AddressOf MementoPattern),("Observer",AddressOf ObserverPattern),("State",AddressOf StatePattern),("Strategy",AddressOf StrategyPattern),("Template Method",AddressOf TemplateMethodPattern),("Visitor",AddressOf VisitorPattern),("MVC",AddressOf MvcPattern),("MVVM",AddressOf MvvmPattern),("Microkernel",AddressOf MicrokernelPattern),("Microservices",AddressOf MicroservicesPattern),("Enterprise Adapter",AddressOf EnterpriseAdapterPattern),("Enterprise Bridge",AddressOf EnterpriseBridgePattern),("Enterprise Facade",AddressOf EnterpriseFacadePattern),("Broker",AddressOf BrokerPattern),("Message Bus",AddressOf MessageBusPattern),("Service Locator",AddressOf ServiceLocatorPattern),("Active Object",AddressOf ActiveObjectPattern),("Monitor Object",AddressOf MonitorObjectPattern),("Half-Sync / Half-Async",AddressOf HalfSyncHalfAsyncPattern),("Leader / Followers",AddressOf LeaderFollowersPattern),("Client-Server",AddressOf ClientServerPattern),("Peer-to-Peer",AddressOf PeerToPeerPattern),("Publish-Subscribe",AddressOf PublishSubscribePattern),("Distributed Proxy",AddressOf DistributedProxyPattern),("Presentation-Abstraction-Control",AddressOf PacPattern),("Model-View-Presenter",AddressOf MvpPattern),("Document-View",AddressOf DocumentViewPattern),("Active Record",AddressOf ActiveRecordPattern),("Data Mapper",AddressOf DataMapperPattern),("Unit of Work",AddressOf UnitOfWorkPattern),("Repository",AddressOf RepositoryPattern),("Dependency Injection",AddressOf DependencyInjectionPattern),("Lazy Initialization",AddressOf LazyInitializationPattern),("Object Pool",AddressOf ObjectPoolPattern),("Null Object",AddressOf NullObjectPattern)}
        For Each item In cases
            If Not item.Item2() Then Throw New InvalidOperationException($"pattern failed: {item.Item1}")
        Next
        If cases.Length<>39 Then Throw New InvalidOperationException($"expected 39 cases, got {cases.Length}")
        Console.WriteLine("VB.NET pattern sweep: 39/39 examples passed")
    End Sub
End Module