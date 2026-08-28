Imports System
Module PatternSweep
    Sub Main()
        Dim cases As (String, Func(Of Boolean))() = {
            ("Command", AddressOf CommandExample.Run), ("Interpreter", AddressOf InterpreterExample.Run), ("Iterator", AddressOf IteratorExample.Run),
            ("Mediator", AddressOf MediatorExample.Run), ("Memento", AddressOf MementoExample.Run), ("Observer", AddressOf ObserverExample.Run),
            ("State", AddressOf StateExample.Run), ("Strategy", AddressOf StrategyExample.Run), ("Template Method", AddressOf TemplateMethodExample.Run),
            ("Visitor", AddressOf VisitorExample.Run), ("MVC", AddressOf MvcExample.Run), ("MVVM", AddressOf MvvmExample.Run),
            ("Microkernel", AddressOf MicrokernelExample.Run), ("Microservices", AddressOf MicroservicesExample.Run),
            ("Enterprise Adapter", AddressOf EnterpriseAdapterExample.Run), ("Enterprise Bridge", AddressOf EnterpriseBridgeExample.Run),
            ("Enterprise Facade", AddressOf EnterpriseFacadeExample.Run), ("Broker", AddressOf BrokerExample.Run),
            ("Message Bus", AddressOf MessageBusExample.Run), ("Service Locator", AddressOf ServiceLocatorExample.Run),
            ("Active Object", AddressOf ActiveObjectExample.Run), ("Monitor Object", AddressOf MonitorObjectExample.Run),
            ("Half-Sync / Half-Async", AddressOf HalfSyncHalfAsyncExample.Run), ("Leader / Followers", AddressOf LeaderFollowersExample.Run),
            ("Client-Server", AddressOf ClientServerExample.Run), ("Peer-to-Peer", AddressOf PeerToPeerExample.Run),
            ("Publish-Subscribe", AddressOf PublishSubscribeExample.Run), ("Distributed Proxy", AddressOf DistributedProxyExample.Run),
            ("Presentation-Abstraction-Control", AddressOf PresentationAbstractionControlExample.Run),
            ("Model-View-Presenter", AddressOf ModelViewPresenterExample.Run), ("Document-View", AddressOf DocumentViewExample.Run),
            ("Active Record", AddressOf ActiveRecordExample.Run), ("Data Mapper", AddressOf DataMapperExample.Run),
            ("Unit of Work", AddressOf UnitOfWorkExample.Run), ("Repository", AddressOf RepositoryExample.Run),
            ("Dependency Injection", AddressOf DependencyInjectionExample.Run), ("Lazy Initialization", AddressOf LazyInitializationExample.Run),
            ("Object Pool", AddressOf ObjectPoolExample.Run), ("Null Object", AddressOf NullObjectExample.Run)}
        For Each item In cases
            If Not item.Item2() Then Throw New InvalidOperationException($"pattern failed: {item.Item1}")
        Next
        If cases.Length <> 39 Then Throw New InvalidOperationException($"expected 39 cases, got {cases.Length}")
        Console.WriteLine("VB.NET pattern sweep: 39/39 examples passed")
    End Sub
End Module
