fun must(value:Boolean,name:String){if(!value)error("pattern failed: $name")}
fun main(){
 val cases=listOf<Pair<String,()->Boolean>>(
  "Command" to CommandExample::run,"Interpreter" to InterpreterExample::run,"Iterator" to IteratorExample::run,"Mediator" to MediatorExample::run,
  "Memento" to MementoExample::run,"Observer" to ObserverExample::run,"State" to StateExample::run,"Strategy" to StrategyExample::run,
  "Template Method" to TemplateMethodExample::run,"Visitor" to VisitorExample::run,"MVC" to MvcExample::run,"MVVM" to MvvmExample::run,
  "Microkernel" to MicrokernelExample::run,"Microservices" to MicroservicesExample::run,"Enterprise Adapter" to EnterpriseAdapterExample::run,
  "Enterprise Bridge" to EnterpriseBridgeExample::run,"Enterprise Facade" to EnterpriseFacadeExample::run,"Broker" to BrokerExample::run,
  "Message Bus" to MessageBusExample::run,"Service Locator" to ServiceLocatorExample::run,"Active Object" to ActiveObjectExample::run,
  "Monitor Object" to MonitorObjectExample::run,"Half-Sync / Half-Async" to HalfSyncHalfAsyncExample::run,"Leader / Followers" to LeaderFollowersExample::run,
  "Client-Server" to ClientServerExample::run,"Peer-to-Peer" to PeerToPeerExample::run,"Publish-Subscribe" to PublishSubscribeExample::run,
  "Distributed Proxy" to DistributedProxyExample::run,"Presentation-Abstraction-Control" to PresentationAbstractionControlExample::run,
  "Model-View-Presenter" to ModelViewPresenterExample::run,"Document-View" to DocumentViewExample::run,"Active Record" to ActiveRecordExample::run,
  "Data Mapper" to DataMapperExample::run,"Unit of Work" to UnitOfWorkExample::run,"Repository" to RepositoryExample::run,
  "Dependency Injection" to DependencyInjectionExample::run,"Lazy Initialization" to LazyInitializationExample::run,
  "Object Pool" to ObjectPoolExample::run,"Null Object" to NullObjectExample::run)
 cases.forEach{(n,f)->must(f(),n)};must(cases.size==39,"count");println("Kotlin pattern sweep: 39/39 examples passed")
}
