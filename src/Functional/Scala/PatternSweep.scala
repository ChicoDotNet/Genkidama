object PatternSweep {
  val cases = List[(String, () => Boolean)](
    ("Command", () => CommandExample.run), ("Interpreter", () => InterpreterExample.run), ("Iterator", () => IteratorExample.run),
    ("Mediator", () => MediatorExample.run), ("Memento", () => MementoExample.run), ("Observer", () => ObserverExample.run),
    ("State", () => StateExample.run), ("Strategy", () => StrategyExample.run), ("Template Method", () => TemplateMethodExample.run),
    ("Visitor", () => VisitorExample.run), ("MVC", () => MvcExample.run), ("MVVM", () => MvvmExample.run),
    ("Microkernel", () => MicrokernelExample.run), ("Microservices", () => MicroservicesExample.run),
    ("Enterprise Adapter", () => EnterpriseAdapterExample.run), ("Enterprise Bridge", () => EnterpriseBridgeExample.run),
    ("Enterprise Facade", () => EnterpriseFacadeExample.run), ("Broker", () => BrokerExample.run),
    ("Message Bus", () => MessageBusExample.run), ("Service Locator", () => ServiceLocatorExample.run),
    ("Active Object", () => ActiveObjectExample.run), ("Monitor Object", () => MonitorObjectExample.run),
    ("Half-Sync / Half-Async", () => HalfSyncHalfAsyncExample.run), ("Leader / Followers", () => LeaderFollowersExample.run),
    ("Client-Server", () => ClientServerExample.run), ("Peer-to-Peer", () => PeerToPeerExample.run),
    ("Publish-Subscribe", () => PublishSubscribeExample.run), ("Distributed Proxy", () => DistributedProxyExample.run),
    ("Presentation-Abstraction-Control", () => PresentationAbstractionControlExample.run),
    ("Model-View-Presenter", () => ModelViewPresenterExample.run), ("Document-View", () => DocumentViewExample.run),
    ("Active Record", () => ActiveRecordExample.run), ("Data Mapper", () => DataMapperExample.run),
    ("Unit of Work", () => UnitOfWorkExample.run), ("Repository", () => RepositoryExample.run),
    ("Dependency Injection", () => DependencyInjectionExample.run), ("Lazy Initialization", () => LazyInitializationExample.run),
    ("Object Pool", () => ObjectPoolExample.run), ("Null Object", () => NullObjectExample.run)
  )
  def main(args: Array[String]): Unit = {
    cases.foreach { case (name, check) => require(check(), s"pattern failed: $name") }
    require(cases.size == 39)
    println("Scala pattern sweep: 39/39 examples passed")
  }
}
