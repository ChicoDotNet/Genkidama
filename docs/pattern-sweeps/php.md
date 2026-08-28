# PHP pattern sweep

> **Scope:** PHP column for the 39 catalog patterns remaining after Chain of Responsibility.  
> **Status:** materialized under hardened KB-006; verification is owned by the reviewed-head PHP 8.5 gate.  
> **Applicability:** **39 Applicable, 0 N/A**.  
> **Promotion boundary:** this ledger certifies PHP cells only; it does not mark any canonical pattern page `validated`.

## Canonical-source rule

Every PHP cell has its own addressable source artifact under [`src/Scripting/PHP/patterns/`](../../src/Scripting/PHP/patterns/). [`pattern_sweep.php`](../../src/Scripting/PHP/pattern_sweep.php) is orchestration only and contains no duplicate pattern implementation.

PHP can express all 39 intents meaningfully through objects, closures/callables, iterators, queues, registries, message boundaries and standard synchronization primitives. No PHP `N/A` is introduced.

## Implementations

| Pattern | Applicability | Canonical source | Validation |
|---|---|---|---|
| Command | Applicable | [`command.php`](../../src/Scripting/PHP/patterns/command.php) | lint + standalone + aggregate |
| Interpreter | Applicable | [`interpreter.php`](../../src/Scripting/PHP/patterns/interpreter.php) | lint + standalone + aggregate |
| Iterator | Applicable | [`iterator.php`](../../src/Scripting/PHP/patterns/iterator.php) | lint + standalone + aggregate |
| Mediator | Applicable | [`mediator.php`](../../src/Scripting/PHP/patterns/mediator.php) | lint + standalone + aggregate |
| Memento | Applicable | [`memento.php`](../../src/Scripting/PHP/patterns/memento.php) | lint + standalone + aggregate |
| Observer | Applicable | [`observer.php`](../../src/Scripting/PHP/patterns/observer.php) | lint + standalone + aggregate |
| State | Applicable | [`state.php`](../../src/Scripting/PHP/patterns/state.php) | lint + standalone + aggregate |
| Strategy | Applicable | [`strategy.php`](../../src/Scripting/PHP/patterns/strategy.php) | lint + standalone + aggregate |
| Template Method | Applicable | [`template_method.php`](../../src/Scripting/PHP/patterns/template_method.php) | lint + standalone + aggregate |
| Visitor | Applicable | [`visitor.php`](../../src/Scripting/PHP/patterns/visitor.php) | lint + standalone + aggregate |
| MVC | Applicable | [`mvc.php`](../../src/Scripting/PHP/patterns/mvc.php) | lint + standalone + aggregate |
| MVVM | Applicable | [`mvvm.php`](../../src/Scripting/PHP/patterns/mvvm.php) | lint + standalone + aggregate |
| Microkernel | Applicable | [`microkernel.php`](../../src/Scripting/PHP/patterns/microkernel.php) | lint + standalone + aggregate |
| Microservices | Applicable | [`microservices.php`](../../src/Scripting/PHP/patterns/microservices.php) | lint + standalone + aggregate |
| Enterprise Adapter | Applicable | [`enterprise_adapter.php`](../../src/Scripting/PHP/patterns/enterprise_adapter.php) | lint + standalone + aggregate |
| Enterprise Bridge | Applicable | [`enterprise_bridge.php`](../../src/Scripting/PHP/patterns/enterprise_bridge.php) | lint + standalone + aggregate |
| Enterprise Facade | Applicable | [`enterprise_facade.php`](../../src/Scripting/PHP/patterns/enterprise_facade.php) | lint + standalone + aggregate |
| Broker | Applicable | [`broker.php`](../../src/Scripting/PHP/patterns/broker.php) | lint + standalone + aggregate |
| Message Bus | Applicable | [`message_bus.php`](../../src/Scripting/PHP/patterns/message_bus.php) | lint + standalone + aggregate |
| Service Locator | Applicable | [`service_locator.php`](../../src/Scripting/PHP/patterns/service_locator.php) | lint + standalone + aggregate |
| Active Object | Applicable | [`active_object.php`](../../src/Scripting/PHP/patterns/active_object.php) | lint + standalone + aggregate |
| Monitor Object | Applicable | [`monitor_object.php`](../../src/Scripting/PHP/patterns/monitor_object.php) | lint + standalone + aggregate |
| Half-Sync / Half-Async | Applicable | [`half_sync_half_async.php`](../../src/Scripting/PHP/patterns/half_sync_half_async.php) | lint + standalone + aggregate |
| Leader / Followers | Applicable | [`leader_followers.php`](../../src/Scripting/PHP/patterns/leader_followers.php) | lint + standalone + aggregate |
| Client-Server | Applicable | [`client_server.php`](../../src/Scripting/PHP/patterns/client_server.php) | lint + standalone + aggregate |
| Peer-to-Peer | Applicable | [`peer_to_peer.php`](../../src/Scripting/PHP/patterns/peer_to_peer.php) | lint + standalone + aggregate |
| Publish-Subscribe | Applicable | [`publish_subscribe.php`](../../src/Scripting/PHP/patterns/publish_subscribe.php) | lint + standalone + aggregate |
| Distributed Proxy | Applicable | [`distributed_proxy.php`](../../src/Scripting/PHP/patterns/distributed_proxy.php) | lint + standalone + aggregate |
| Presentation-Abstraction-Control | Applicable | [`presentation_abstraction_control.php`](../../src/Scripting/PHP/patterns/presentation_abstraction_control.php) | lint + standalone + aggregate |
| Model-View-Presenter | Applicable | [`model_view_presenter.php`](../../src/Scripting/PHP/patterns/model_view_presenter.php) | lint + standalone + aggregate |
| Document-View | Applicable | [`document_view.php`](../../src/Scripting/PHP/patterns/document_view.php) | lint + standalone + aggregate |
| Active Record | Applicable | [`active_record.php`](../../src/Scripting/PHP/patterns/active_record.php) | lint + standalone + aggregate |
| Data Mapper | Applicable | [`data_mapper.php`](../../src/Scripting/PHP/patterns/data_mapper.php) | lint + standalone + aggregate |
| Unit of Work | Applicable | [`unit_of_work.php`](../../src/Scripting/PHP/patterns/unit_of_work.php) | lint + standalone + aggregate |
| Repository | Applicable | [`repository.php`](../../src/Scripting/PHP/patterns/repository.php) | lint + standalone + aggregate |
| Dependency Injection | Applicable | [`dependency_injection.php`](../../src/Scripting/PHP/patterns/dependency_injection.php) | lint + standalone + aggregate |
| Lazy Initialization | Applicable | [`lazy_initialization.php`](../../src/Scripting/PHP/patterns/lazy_initialization.php) | lint + standalone + aggregate |
| Object Pool | Applicable | [`object_pool.php`](../../src/Scripting/PHP/patterns/object_pool.php) | lint + standalone + aggregate |
| Null Object | Applicable | [`null_object.php`](../../src/Scripting/PHP/patterns/null_object.php) | lint + standalone + aggregate |

## Validation contract

The target workflow uses PHP **8.5**, lints every canonical file, executes every canonical file independently, verifies exactly 39 cells, then lints and runs the aggregate orchestrator. The aggregate must end with `php-pattern-sweep: 39/39 passed`.

Materialization is not verification. The reviewed head is called verified only after this gate and generic repository CI are green.

## Coverage

No synthetic percentage is assigned to these standalone pedagogical examples. PHP lint plus independent behavioral execution and aggregate execution are the strongest lightweight evidence here; KB-006's >=44% floor remains applicable where meaningful coverage instrumentation exists.

## Cross-lane safety

This slice changes no `learn/**`, course metadata/navigation, course applications, or production architecture. PHP Learn remains independent from this Design Pattern matrix work.
