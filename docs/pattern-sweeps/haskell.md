# Haskell language-major Design Pattern sweep

> **Target:** Haskell  
> **Scope:** the 39 catalog patterns that remained incomplete after Chain of Responsibility  
> **Cohort:** Haskell, Dart, Crystal, Zig, Julia, Go and Objective-C  
> **Pattern completion:** unchanged — this ledger records Haskell cells only; no partial pattern becomes `validated` from this sweep alone.

## Applicability

All 39 remaining patterns are **Applicable** to Haskell. Algebraic data types, pure functions, higher-order functions, immutable snapshots, explicit state transitions and protocol models preserve the design intent without requiring class-oriented syntax.

No Haskell `N/A` classifications are introduced by this sweep. The concurrency examples intentionally validate coordination protocols deterministically rather than claiming scheduler, race or throughput coverage.

## Implementation boundary

[`pattern_sweep.hs`](../../src/Functional/Haskell/pattern_sweep.hs) contains one named executable example for every cell below. Each example validates an observable design behavior; it is not a table of hard-coded pass flags.

| Family | Pattern | Example | Observable intent |
|---|---|---|---|
| Behavioral | Command | `commandExample` | queued operations, ordered execution and undo |
| Behavioral | Interpreter | `interpreterExample` | recursive expression interpretation |
| Behavioral | Iterator | `iteratorExample` | explicit cursor and exhaustion |
| Behavioral | Mediator | `mediatorExample` | colleague routing through a mediator |
| Behavioral | Memento | `mementoExample` | snapshot and restoration |
| Behavioral | Observer | `observerExample` | independent subscriber notification |
| Behavioral | State | `stateExample` | state-dependent behavior and transition |
| Behavioral | Strategy | `strategyExample` | interchangeable algorithms |
| Behavioral | Template Method | `templateMethodExample` | fixed workflow with variable step |
| Behavioral | Visitor | `visitorExample` | independent operations over heterogeneous values |
| Architectural | MVC | `mvcExample` | model/controller/view separation |
| Architectural | MVVM | `mvvmExample` | model projection through a view-model |
| Architectural | Microkernel | `microkernelExample` | small core with registered plugins |
| Architectural | Microservices | `microservicesExample` | independent service contracts coordinated at a boundary |
| Integration | Enterprise Adapter | `enterpriseAdapterExample` | legacy-to-canonical contract translation |
| Integration | Enterprise Bridge | `enterpriseBridgeExample` | abstraction independent from transport |
| Integration | Enterprise Facade | `enterpriseFacadeExample` | one operation coordinating subsystems |
| Integration | Broker | `brokerExample` | registry-mediated service invocation |
| Integration | Message Bus | `messageBusExample` | common message delivery to handlers |
| Integration | Service Locator | `serviceLocatorExample` | runtime dependency lookup |
| Concurrency | Active Object | `activeObjectExample` | enqueue separated from scheduled execution |
| Concurrency | Monitor Object | `monitorObjectExample` | serialized operations over protected state |
| Concurrency | Half-Sync / Half-Async | `halfSyncHalfAsyncExample` | asynchronous arrival feeding synchronous processing |
| Concurrency | Leader / Followers | `leaderFollowersExample` | leader rotation in a handler pool |
| Distribution | Client-Server | `clientServerExample` | client boundary around centralized handling |
| Distribution | Peer-to-Peer | `peerToPeerExample` | symmetric send/receive roles |
| Distribution | Publish-Subscribe | `publishSubscribeExample` | topic-based independent subscriptions |
| Distribution | Distributed Proxy | `distributedProxyExample` | local call hiding remote mechanics |
| Presentation | Presentation-Abstraction-Control | `presentationAbstractionControlExample` | separated presentation, abstraction and control |
| Presentation | Model-View-Presenter | `modelViewPresenterExample` | presenter mediating model and passive view |
| Presentation | Document-View | `documentViewExample` | multiple projections over one document |
| Persistence | Active Record | `activeRecordExample` | record coupled with persistence operation |
| Persistence | Data Mapper | `dataMapperExample` | mapping domain values to persistence rows |
| Persistence | Unit of Work | `unitOfWorkExample` | staged changes committed together |
| Persistence | Repository | `repositoryExample` | collection-like domain access over storage |
| Additional | Dependency Injection | `dependencyInjectionExample` | dependency supplied from outside |
| Additional | Lazy Initialization | `lazyInitializationExample` | create-on-first-use and reuse |
| Additional | Object Pool | `objectPoolExample` | acquire, release and reuse bounded resources |
| Additional | Null Object | `nullObjectExample` | behavior-compatible no-op dependency |

## Validation boundary

The target gate compiles with `ghc -Wall -Werror` and executes the harness. Success must print exactly `Haskell pattern sweep: 39/39 examples passed`.

Until that gate runs on the reviewed PR head, these cells are **materialized but not yet claimed verified**. Code/test coverage is N/A for this educational single-file harness because no meaningful line-coverage gate is configured; behavioral compilation/execution is the stronger relevant evidence.
