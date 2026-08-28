# Ruby pattern sweep

> **Scope:** Ruby column for the 39 catalog patterns remaining after Chain of Responsibility.  
> **Status:** canonicalized under hardened KB-006; verification is owned by the reviewed-head Ruby stable gate.  
> **Applicability:** **39 Applicable, 0 N/A**.  
> **Promotion boundary:** this ledger certifies Ruby cells only; it does not mark any canonical pattern page `validated`.

## Canonical-source rule

Every Ruby cell has its own addressable source artifact under [`src/Scripting/Ruby/patterns/`](../../src/Scripting/Ruby/patterns/). [`pattern_sweep.rb`](../../src/Scripting/Ruby/pattern_sweep.rb) is an orchestrator only: it requires the canonical files, invokes each exported `run`, and contains no duplicate pattern implementation.

Ruby can express all 39 intents meaningfully through objects, modules, blocks, lambdas, Enumerators, duck typing, message passing, threads/queues and standard synchronization primitives. Therefore this slice introduces no Ruby `N/A`.

## Implementations

| Pattern | Applicability | Canonical source | Validation |
|---|---|---|---|
| Command | Applicable | [`command.rb`](../../src/Scripting/Ruby/patterns/command.rb) | syntax + standalone execution + aggregate |
| Interpreter | Applicable | [`interpreter.rb`](../../src/Scripting/Ruby/patterns/interpreter.rb) | syntax + standalone execution + aggregate |
| Iterator | Applicable | [`iterator.rb`](../../src/Scripting/Ruby/patterns/iterator.rb) | syntax + standalone execution + aggregate |
| Mediator | Applicable | [`mediator.rb`](../../src/Scripting/Ruby/patterns/mediator.rb) | syntax + standalone execution + aggregate |
| Memento | Applicable | [`memento.rb`](../../src/Scripting/Ruby/patterns/memento.rb) | syntax + standalone execution + aggregate |
| Observer | Applicable | [`observer.rb`](../../src/Scripting/Ruby/patterns/observer.rb) | syntax + standalone execution + aggregate |
| State | Applicable | [`state.rb`](../../src/Scripting/Ruby/patterns/state.rb) | syntax + standalone execution + aggregate |
| Strategy | Applicable | [`strategy.rb`](../../src/Scripting/Ruby/patterns/strategy.rb) | syntax + standalone execution + aggregate |
| Template Method | Applicable | [`template_method.rb`](../../src/Scripting/Ruby/patterns/template_method.rb) | syntax + standalone execution + aggregate |
| Visitor | Applicable | [`visitor.rb`](../../src/Scripting/Ruby/patterns/visitor.rb) | syntax + standalone execution + aggregate |
| MVC | Applicable | [`mvc.rb`](../../src/Scripting/Ruby/patterns/mvc.rb) | syntax + standalone execution + aggregate |
| MVVM | Applicable | [`mvvm.rb`](../../src/Scripting/Ruby/patterns/mvvm.rb) | syntax + standalone execution + aggregate |
| Microkernel | Applicable | [`microkernel.rb`](../../src/Scripting/Ruby/patterns/microkernel.rb) | syntax + standalone execution + aggregate |
| Microservices | Applicable | [`microservices.rb`](../../src/Scripting/Ruby/patterns/microservices.rb) | syntax + standalone execution + aggregate |
| Enterprise Adapter | Applicable | [`enterprise_adapter.rb`](../../src/Scripting/Ruby/patterns/enterprise_adapter.rb) | syntax + standalone execution + aggregate |
| Enterprise Bridge | Applicable | [`enterprise_bridge.rb`](../../src/Scripting/Ruby/patterns/enterprise_bridge.rb) | syntax + standalone execution + aggregate |
| Enterprise Facade | Applicable | [`enterprise_facade.rb`](../../src/Scripting/Ruby/patterns/enterprise_facade.rb) | syntax + standalone execution + aggregate |
| Broker | Applicable | [`broker.rb`](../../src/Scripting/Ruby/patterns/broker.rb) | syntax + standalone execution + aggregate |
| Message Bus | Applicable | [`message_bus.rb`](../../src/Scripting/Ruby/patterns/message_bus.rb) | syntax + standalone execution + aggregate |
| Service Locator | Applicable | [`service_locator.rb`](../../src/Scripting/Ruby/patterns/service_locator.rb) | syntax + standalone execution + aggregate |
| Active Object | Applicable | [`active_object.rb`](../../src/Scripting/Ruby/patterns/active_object.rb) | syntax + standalone execution + aggregate |
| Monitor Object | Applicable | [`monitor_object.rb`](../../src/Scripting/Ruby/patterns/monitor_object.rb) | syntax + standalone execution + aggregate |
| Half-Sync / Half-Async | Applicable | [`half_sync_half_async.rb`](../../src/Scripting/Ruby/patterns/half_sync_half_async.rb) | syntax + standalone execution + aggregate |
| Leader / Followers | Applicable | [`leader_followers.rb`](../../src/Scripting/Ruby/patterns/leader_followers.rb) | syntax + standalone execution + aggregate |
| Client-Server | Applicable | [`client_server.rb`](../../src/Scripting/Ruby/patterns/client_server.rb) | syntax + standalone execution + aggregate |
| Peer-to-Peer | Applicable | [`peer_to_peer.rb`](../../src/Scripting/Ruby/patterns/peer_to_peer.rb) | syntax + standalone execution + aggregate |
| Publish-Subscribe | Applicable | [`publish_subscribe.rb`](../../src/Scripting/Ruby/patterns/publish_subscribe.rb) | syntax + standalone execution + aggregate |
| Distributed Proxy | Applicable | [`distributed_proxy.rb`](../../src/Scripting/Ruby/patterns/distributed_proxy.rb) | syntax + standalone execution + aggregate |
| Presentation-Abstraction-Control | Applicable | [`presentation_abstraction_control.rb`](../../src/Scripting/Ruby/patterns/presentation_abstraction_control.rb) | syntax + standalone execution + aggregate |
| Model-View-Presenter | Applicable | [`model_view_presenter.rb`](../../src/Scripting/Ruby/patterns/model_view_presenter.rb) | syntax + standalone execution + aggregate |
| Document-View | Applicable | [`document_view.rb`](../../src/Scripting/Ruby/patterns/document_view.rb) | syntax + standalone execution + aggregate |
| Active Record | Applicable | [`active_record.rb`](../../src/Scripting/Ruby/patterns/active_record.rb) | syntax + standalone execution + aggregate |
| Data Mapper | Applicable | [`data_mapper.rb`](../../src/Scripting/Ruby/patterns/data_mapper.rb) | syntax + standalone execution + aggregate |
| Unit of Work | Applicable | [`unit_of_work.rb`](../../src/Scripting/Ruby/patterns/unit_of_work.rb) | syntax + standalone execution + aggregate |
| Repository | Applicable | [`repository.rb`](../../src/Scripting/Ruby/patterns/repository.rb) | syntax + standalone execution + aggregate |
| Dependency Injection | Applicable | [`dependency_injection.rb`](../../src/Scripting/Ruby/patterns/dependency_injection.rb) | syntax + standalone execution + aggregate |
| Lazy Initialization | Applicable | [`lazy_initialization.rb`](../../src/Scripting/Ruby/patterns/lazy_initialization.rb) | syntax + standalone execution + aggregate |
| Object Pool | Applicable | [`object_pool.rb`](../../src/Scripting/Ruby/patterns/object_pool.rb) | syntax + standalone execution + aggregate |
| Null Object | Applicable | [`null_object.rb`](../../src/Scripting/Ruby/patterns/null_object.rb) | syntax + standalone execution + aggregate |

## Validation contract

The target workflow resolves the current stable Ruby through `ruby/setup-ruby`, prints the resolved runtime version, then for all 39 canonical files:

1. runs `ruby -c`;
2. executes the file standalone;
3. verifies that exactly 39 canonical files were exercised;
4. syntax-checks and executes the aggregate runner.

The aggregate must end with:

```text
ruby-pattern-sweep: 39/39 passed
```

Materialization is not verification. The reviewed head is called verified only after this gate and applicable generic repository CI are green.

## Coverage

No synthetic percentage is assigned to these standalone pedagogical examples because there is no useful homogeneous line-coverage denominator. Syntax plus independent behavioral execution and aggregate execution are the strongest lightweight evidence here; KB-006's >=44% policy remains applicable where meaningful coverage instrumentation exists.

## Cross-lane safety

This slice changes no `learn/**`, course metadata/navigation, course applications, or production architecture. Ruby Learn remains independent from this Design Pattern matrix work.
