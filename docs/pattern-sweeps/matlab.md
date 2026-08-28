# MATLAB language-major Design Pattern sweep

> **Target:** MATLAB  
> **Scope:** the 39 catalog patterns that remained incomplete after Chain of Responsibility  
> **Scheduling unit:** one target-language slice, one MATLAB runtime setup, one behavioral validator  
> **Pattern completion:** unchanged — this ledger records MATLAB cells only; no partial pattern becomes `validated` from this sweep alone.

## Why this slice exists

The matrix experiment optimizes **useful pattern work per CI validation cycle**. The first Command-only MATLAB run established a useful baseline: MATLAB setup took 92 seconds while the Command behavioral validation took 6 seconds, for 98 seconds total. Paying that setup once per pattern would make CI, rather than implementation, the dominant cost.

This sweep therefore materializes every remaining MATLAB cell before publishing the next MATLAB validation boundary. `validate_pattern_sweep.m` executes all 39 examples in one MATLAB process after one runtime setup.

A cell is verified only when the current PR head has a green `Pattern MATLAB Sweep` check. This file intentionally does not freeze a historical run number into the source; the check on the reviewed head is the evidence.

## Applicability

All 39 remaining patterns are **Applicable** to MATLAB. MATLAB functions, structs, function handles, arrays, explicit state, message-like values and dispatch tables can preserve their intents without requiring textbook class syntax.

No MATLAB `N/A` classifications are introduced by this sweep.

For concurrency patterns the examples validate the coordination protocol deterministically. They do **not** claim thread-race or throughput testing, because that would require disproportionate Parallel Computing Toolbox infrastructure for these teaching examples.

## Cells

| Family | Pattern | MATLAB example | What the validator observes |
|---|---|---|---|
| Behavioral | Command | [`command.m`](../../src/DataScience/MATLAB/command.m) | queued requests, execution order and undo |
| Behavioral | Interpreter | [`interpreter.m`](../../src/DataScience/MATLAB/interpreter.m) | recursive AST evaluation |
| Behavioral | Iterator | [`iterator.m`](../../src/DataScience/MATLAB/iterator.m) | explicit cursor traversal and exhaustion |
| Behavioral | Mediator | [`mediator.m`](../../src/DataScience/MATLAB/mediator.m) | colleague coordination through mediator routing |
| Behavioral | Memento | [`memento.m`](../../src/DataScience/MATLAB/memento.m) | snapshot and state restoration |
| Behavioral | Observer | [`observer.m`](../../src/DataScience/MATLAB/observer.m) | independent subscriber notifications |
| Behavioral | State | [`state.m`](../../src/DataScience/MATLAB/state.m) | state-dependent transitions and behavior |
| Behavioral | Strategy | [`strategy.m`](../../src/DataScience/MATLAB/strategy.m) | interchangeable pricing algorithms |
| Behavioral | Template Method | [`template_method.m`](../../src/DataScience/MATLAB/template_method.m) | fixed algorithm skeleton with variable steps |
| Behavioral | Visitor | [`visitor.m`](../../src/DataScience/MATLAB/visitor.m) | multiple operations over heterogeneous shapes |
| Architectural | MVC | [`mvc.m`](../../src/DataScience/MATLAB/mvc.m) | model/controller/view separation |
| Architectural | MVVM | [`mvvm.m`](../../src/DataScience/MATLAB/mvvm.m) | view-model projection and command boundary |
| Architectural | Microkernel | [`microkernel.m`](../../src/DataScience/MATLAB/microkernel.m) | small core with registered plugins |
| Architectural | Microservices | [`microservices.m`](../../src/DataScience/MATLAB/microservices.m) | independent service contracts and coordination |
| Integration | Enterprise Adapter | [`enterprise_adapter.m`](../../src/DataScience/MATLAB/enterprise_adapter.m) | legacy-to-canonical contract translation |
| Integration | Enterprise Bridge | [`enterprise_bridge.m`](../../src/DataScience/MATLAB/enterprise_bridge.m) | independent abstraction and transport dimensions |
| Integration | Enterprise Facade | [`enterprise_facade.m`](../../src/DataScience/MATLAB/enterprise_facade.m) | one operation coordinating integration subsystems |
| Integration | Broker | [`broker.m`](../../src/DataScience/MATLAB/broker.m) | intermediary service lookup and routing |
| Integration | Message Bus | [`message_bus.m`](../../src/DataScience/MATLAB/message_bus.m) | common message delivery to independent handlers |
| Integration | Service Locator | [`service_locator.m`](../../src/DataScience/MATLAB/service_locator.m) | runtime dependency lookup from a registry |
| Concurrency | Active Object | [`active_object.m`](../../src/DataScience/MATLAB/active_object.m) | queued invocation separated from scheduled execution |
| Concurrency | Monitor Object | [`monitor_object.m`](../../src/DataScience/MATLAB/monitor_object.m) | serialized access through monitor operations |
| Concurrency | Half-Sync / Half-Async | [`half_sync_half_async.m`](../../src/DataScience/MATLAB/half_sync_half_async.m) | async-arrival queue feeding synchronous processing |
| Concurrency | Leader / Followers | [`leader_followers.m`](../../src/DataScience/MATLAB/leader_followers.m) | leadership rotation across an event-handling pool |
| Distribution | Client-Server | [`client_server.m`](../../src/DataScience/MATLAB/client_server.m) | client request separated from centralized handling |
| Distribution | Peer-to-Peer | [`peer_to_peer.m`](../../src/DataScience/MATLAB/peer_to_peer.m) | peers both originate and receive data |
| Distribution | Publish-Subscribe | [`publish_subscribe.m`](../../src/DataScience/MATLAB/publish_subscribe.m) | topic publication to independent subscribers |
| Distribution | Distributed Proxy | [`distributed_proxy.m`](../../src/DataScience/MATLAB/distributed_proxy.m) | local proxy hiding remote-call mechanics |
| Presentation | Presentation-Abstraction-Control | [`presentation_abstraction_control.m`](../../src/DataScience/MATLAB/presentation_abstraction_control.m) | agent presentation, abstraction and control separation |
| Presentation | Model-View-Presenter | [`model_view_presenter.m`](../../src/DataScience/MATLAB/model_view_presenter.m) | presenter mediating model and passive view |
| Presentation | Document-View | [`document_view.m`](../../src/DataScience/MATLAB/document_view.m) | multiple views over one document model |
| Persistence | Active Record | [`active_record.m`](../../src/DataScience/MATLAB/active_record.m) | record data coupled with persistence operations |
| Persistence | Data Mapper | [`data_mapper.m`](../../src/DataScience/MATLAB/data_mapper.m) | domain/persistence representation separation |
| Persistence | Unit of Work | [`unit_of_work.m`](../../src/DataScience/MATLAB/unit_of_work.m) | staged changes committed as one unit |
| Persistence | Repository | [`repository.m`](../../src/DataScience/MATLAB/repository.m) | collection-like domain access over storage |
| Additional | Dependency Injection | [`dependency_injection.m`](../../src/DataScience/MATLAB/dependency_injection.m) | externally supplied interchangeable dependency |
| Additional | Lazy Initialization | [`lazy_initialization.m`](../../src/DataScience/MATLAB/lazy_initialization.m) | first-use creation exactly once |
| Additional | Object Pool | [`object_pool.m`](../../src/DataScience/MATLAB/object_pool.m) | acquire, release and reuse of bounded resources |
| Additional | Null Object | [`null_object.m`](../../src/DataScience/MATLAB/null_object.m) | behavior-compatible no-op dependency |

## Validation boundary

[`validate_pattern_sweep.m`](../../src/DataScience/MATLAB/validate_pattern_sweep.m) is the target-level validator. It executes every cell above and asserts observable behavior rather than names or inheritance shapes.

The corresponding workflow installs MATLAB **once** and then runs that validator. Timing telemetry records `cells`, `setup_seconds`, `validation_seconds` and `total_seconds` so future language ordering can be selected from observed CI economics.

A green sweep certifies the MATLAB column for these 39 patterns. It does not certify their other language cells, their final KB-006 page completeness, or promotion readiness of any individual pattern.
