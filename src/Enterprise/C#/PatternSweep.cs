using System;
using Genkidama.PatternExamples;

public static class PatternSweep
{
    public static void Main()
    {
        (string Name, Func<bool> Check)[] cases =
        {
            ("Command", CommandExample.Run), ("Interpreter", InterpreterExample.Run), ("Iterator", IteratorExample.Run),
            ("Mediator", MediatorExample.Run), ("Memento", MementoExample.Run), ("Observer", ObserverExample.Run),
            ("State", StateExample.Run), ("Strategy", StrategyExample.Run), ("Template Method", TemplateMethodExample.Run),
            ("Visitor", VisitorExample.Run), ("MVC", MvcExample.Run), ("MVVM", MvvmExample.Run),
            ("Microkernel", MicrokernelExample.Run), ("Microservices", MicroservicesExample.Run),
            ("Enterprise Adapter", EnterpriseAdapterExample.Run), ("Enterprise Bridge", EnterpriseBridgeExample.Run),
            ("Enterprise Facade", EnterpriseFacadeExample.Run), ("Broker", BrokerExample.Run),
            ("Message Bus", MessageBusExample.Run), ("Service Locator", ServiceLocatorExample.Run),
            ("Active Object", ActiveObjectExample.Run), ("Monitor Object", MonitorObjectExample.Run),
            ("Half-Sync / Half-Async", HalfSyncHalfAsyncExample.Run), ("Leader / Followers", LeaderFollowersExample.Run),
            ("Client-Server", ClientServerExample.Run), ("Peer-to-Peer", PeerToPeerExample.Run),
            ("Publish-Subscribe", PublishSubscribeExample.Run), ("Distributed Proxy", DistributedProxyExample.Run),
            ("Presentation-Abstraction-Control", PresentationAbstractionControlExample.Run),
            ("Model-View-Presenter", ModelViewPresenterExample.Run), ("Document-View", DocumentViewExample.Run),
            ("Active Record", ActiveRecordExample.Run), ("Data Mapper", DataMapperExample.Run),
            ("Unit of Work", UnitOfWorkExample.Run), ("Repository", RepositoryExample.Run),
            ("Dependency Injection", DependencyInjectionExample.Run), ("Lazy Initialization", LazyInitializationExample.Run),
            ("Object Pool", ObjectPoolExample.Run), ("Null Object", NullObjectExample.Run)
        };
        foreach (var (name, check) in cases)
            if (!check()) throw new InvalidOperationException($"pattern failed: {name}");
        if (cases.Length != 39) throw new InvalidOperationException($"expected 39 cases, got {cases.Length}");
        Console.WriteLine("C# pattern sweep: 39/39 examples passed");
    }
}
