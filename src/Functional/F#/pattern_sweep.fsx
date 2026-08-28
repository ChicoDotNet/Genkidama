#load "patterns/Command.fsx"
#load "patterns/Interpreter.fsx"
#load "patterns/Iterator.fsx"
#load "patterns/Mediator.fsx"
#load "patterns/Memento.fsx"
#load "patterns/Observer.fsx"
#load "patterns/State.fsx"
#load "patterns/Strategy.fsx"
#load "patterns/TemplateMethod.fsx"
#load "patterns/Visitor.fsx"
#load "patterns/MVC.fsx"
#load "patterns/MVVM.fsx"
#load "patterns/Microkernel.fsx"
#load "patterns/Microservices.fsx"
#load "patterns/EnterpriseAdapter.fsx"
#load "patterns/EnterpriseBridge.fsx"
#load "patterns/EnterpriseFacade.fsx"
#load "patterns/Broker.fsx"
#load "patterns/MessageBus.fsx"
#load "patterns/ServiceLocator.fsx"
#load "patterns/ActiveObject.fsx"
#load "patterns/MonitorObject.fsx"
#load "patterns/HalfSyncHalfAsync.fsx"
#load "patterns/LeaderFollowers.fsx"
#load "patterns/ClientServer.fsx"
#load "patterns/PeerToPeer.fsx"
#load "patterns/PublishSubscribe.fsx"
#load "patterns/DistributedProxy.fsx"
#load "patterns/PresentationAbstractionControl.fsx"
#load "patterns/ModelViewPresenter.fsx"
#load "patterns/DocumentView.fsx"
#load "patterns/ActiveRecord.fsx"
#load "patterns/DataMapper.fsx"
#load "patterns/UnitOfWork.fsx"
#load "patterns/Repository.fsx"
#load "patterns/DependencyInjection.fsx"
#load "patterns/LazyInitialization.fsx"
#load "patterns/ObjectPool.fsx"
#load "patterns/NullObject.fsx"

let cases : (string * (unit -> bool)) list = [
    "Command", CommandExample.run; "Interpreter", InterpreterExample.run; "Iterator", IteratorExample.run; "Mediator", MediatorExample.run;
    "Memento", MementoExample.run; "Observer", ObserverExample.run; "State", StateExample.run; "Strategy", StrategyExample.run;
    "Template Method", TemplateMethodExample.run; "Visitor", VisitorExample.run; "MVC", MvcExample.run; "MVVM", MvvmExample.run;
    "Microkernel", MicrokernelExample.run; "Microservices", MicroservicesExample.run; "Enterprise Adapter", EnterpriseAdapterExample.run;
    "Enterprise Bridge", EnterpriseBridgeExample.run; "Enterprise Facade", EnterpriseFacadeExample.run; "Broker", BrokerExample.run;
    "Message Bus", MessageBusExample.run; "Service Locator", ServiceLocatorExample.run; "Active Object", ActiveObjectExample.run;
    "Monitor Object", MonitorObjectExample.run; "Half-Sync / Half-Async", HalfSyncHalfAsyncExample.run; "Leader / Followers", LeaderFollowersExample.run;
    "Client-Server", ClientServerExample.run; "Peer-to-Peer", PeerToPeerExample.run; "Publish-Subscribe", PublishSubscribeExample.run;
    "Distributed Proxy", DistributedProxyExample.run; "Presentation-Abstraction-Control", PresentationAbstractionControlExample.run;
    "Model-View-Presenter", ModelViewPresenterExample.run; "Document-View", DocumentViewExample.run; "Active Record", ActiveRecordExample.run;
    "Data Mapper", DataMapperExample.run; "Unit of Work", UnitOfWorkExample.run; "Repository", RepositoryExample.run;
    "Dependency Injection", DependencyInjectionExample.run; "Lazy Initialization", LazyInitializationExample.run;
    "Object Pool", ObjectPoolExample.run; "Null Object", NullObjectExample.run ]
for name, check in cases do if not (check ()) then failwith $"pattern failed: {name}"
if cases.Length <> 39 then failwith $"expected 39 cases, got {cases.Length}"
printfn "F# pattern sweep: 39/39 examples passed"
