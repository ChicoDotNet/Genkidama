type Check = [string, () => boolean];
const cases: Check[] = [
  ['Command', commandPattern], ['Interpreter', interpreterPattern], ['Iterator', iteratorPattern], ['Mediator', mediatorPattern],
  ['Memento', mementoPattern], ['Observer', observerPattern], ['State', statePattern], ['Strategy', strategyPattern],
  ['Template Method', templateMethodPattern], ['Visitor', visitorPattern], ['MVC', mvcPattern], ['MVVM', mvvmPattern],
  ['Microkernel', microkernelPattern], ['Microservices', microservicesPattern], ['Enterprise Adapter', enterpriseAdapterPattern],
  ['Enterprise Bridge', enterpriseBridgePattern], ['Enterprise Facade', enterpriseFacadePattern], ['Broker', brokerPattern],
  ['Message Bus', messageBusPattern], ['Service Locator', serviceLocatorPattern], ['Active Object', activeObjectPattern],
  ['Monitor Object', monitorObjectPattern], ['Half-Sync / Half-Async', halfSyncHalfAsyncPattern], ['Leader / Followers', leaderFollowersPattern],
  ['Client-Server', clientServerPattern], ['Peer-to-Peer', peerToPeerPattern], ['Publish-Subscribe', publishSubscribePattern],
  ['Distributed Proxy', distributedProxyPattern], ['Presentation-Abstraction-Control', pacPattern], ['Model-View-Presenter', mvpPattern],
  ['Document-View', documentViewPattern], ['Active Record', activeRecordPattern], ['Data Mapper', dataMapperPattern],
  ['Unit of Work', unitOfWorkPattern], ['Repository', repositoryPattern], ['Dependency Injection', dependencyInjectionPattern],
  ['Lazy Initialization', lazyInitializationPattern], ['Object Pool', objectPoolPattern], ['Null Object', nullObjectPattern]
];
for (const [name, check] of cases) if (!check()) throw new Error(`pattern failed: ${name}`);
if (cases.length !== 39) throw new Error(`expected 39 cases, got ${cases.length}`);
console.log('TypeScript pattern sweep: 39/39 examples passed');
