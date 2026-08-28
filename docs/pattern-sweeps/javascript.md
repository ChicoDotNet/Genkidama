# JavaScript pattern sweep

> **Scope:** JavaScript column for the 39 catalog patterns remaining after Chain of Responsibility.  
> **Status:** materialized under hardened KB-006; verification is owned by the reviewed-head Node LTS gate.  
> **Applicability:** **39 Applicable, 0 N/A**.  
> **Promotion boundary:** this ledger certifies JavaScript cells only; it does not mark any canonical pattern page `validated`.

## Canonical-source rule

Every JavaScript cell has its own addressable source artifact under [`src/Web/JavaScriptJS/patterns/`](../../src/Web/JavaScriptJS/patterns/). [`pattern_sweep.js`](../../src/Web/JavaScriptJS/pattern_sweep.js) is an orchestrator only: it loads the canonical files, invokes each exported `run`, and contains no duplicate pattern implementation.

JavaScript can express all 39 intents meaningfully through objects, functions, closures, iterables, prototypes/classes, promises, queues, maps/sets and explicit message boundaries. Therefore this slice introduces no JavaScript `N/A`.

## Implementations

| Pattern | Applicability | Canonical source | Validation |
|---|---|---|---|
| Command | Applicable | [`command.js`](../../src/Web/JavaScriptJS/patterns/command.js) | syntax + standalone execution + aggregate |
| Interpreter | Applicable | [`interpreter.js`](../../src/Web/JavaScriptJS/patterns/interpreter.js) | syntax + standalone execution + aggregate |
| Iterator | Applicable | [`iterator.js`](../../src/Web/JavaScriptJS/patterns/iterator.js) | syntax + standalone execution + aggregate |
| Mediator | Applicable | [`mediator.js`](../../src/Web/JavaScriptJS/patterns/mediator.js) | syntax + standalone execution + aggregate |
| Memento | Applicable | [`memento.js`](../../src/Web/JavaScriptJS/patterns/memento.js) | syntax + standalone execution + aggregate |
| Observer | Applicable | [`observer.js`](../../src/Web/JavaScriptJS/patterns/observer.js) | syntax + standalone execution + aggregate |
| State | Applicable | [`state.js`](../../src/Web/JavaScriptJS/patterns/state.js) | syntax + standalone execution + aggregate |
| Strategy | Applicable | [`strategy.js`](../../src/Web/JavaScriptJS/patterns/strategy.js) | syntax + standalone execution + aggregate |
| Template Method | Applicable | [`template_method.js`](../../src/Web/JavaScriptJS/patterns/template_method.js) | syntax + standalone execution + aggregate |
| Visitor | Applicable | [`visitor.js`](../../src/Web/JavaScriptJS/patterns/visitor.js) | syntax + standalone execution + aggregate |
| MVC | Applicable | [`mvc.js`](../../src/Web/JavaScriptJS/patterns/mvc.js) | syntax + standalone execution + aggregate |
| MVVM | Applicable | [`mvvm.js`](../../src/Web/JavaScriptJS/patterns/mvvm.js) | syntax + standalone execution + aggregate |
| Microkernel | Applicable | [`microkernel.js`](../../src/Web/JavaScriptJS/patterns/microkernel.js) | syntax + standalone execution + aggregate |
| Microservices | Applicable | [`microservices.js`](../../src/Web/JavaScriptJS/patterns/microservices.js) | syntax + standalone execution + aggregate |
| Enterprise Adapter | Applicable | [`enterprise_adapter.js`](../../src/Web/JavaScriptJS/patterns/enterprise_adapter.js) | syntax + standalone execution + aggregate |
| Enterprise Bridge | Applicable | [`enterprise_bridge.js`](../../src/Web/JavaScriptJS/patterns/enterprise_bridge.js) | syntax + standalone execution + aggregate |
| Enterprise Facade | Applicable | [`enterprise_facade.js`](../../src/Web/JavaScriptJS/patterns/enterprise_facade.js) | syntax + standalone execution + aggregate |
| Broker | Applicable | [`broker.js`](../../src/Web/JavaScriptJS/patterns/broker.js) | syntax + standalone execution + aggregate |
| Message Bus | Applicable | [`message_bus.js`](../../src/Web/JavaScriptJS/patterns/message_bus.js) | syntax + standalone execution + aggregate |
| Service Locator | Applicable | [`service_locator.js`](../../src/Web/JavaScriptJS/patterns/service_locator.js) | syntax + standalone execution + aggregate |
| Active Object | Applicable | [`active_object.js`](../../src/Web/JavaScriptJS/patterns/active_object.js) | syntax + standalone execution + aggregate |
| Monitor Object | Applicable | [`monitor_object.js`](../../src/Web/JavaScriptJS/patterns/monitor_object.js) | syntax + standalone execution + aggregate |
| Half-Sync / Half-Async | Applicable | [`half_sync_half_async.js`](../../src/Web/JavaScriptJS/patterns/half_sync_half_async.js) | syntax + standalone execution + aggregate |
| Leader / Followers | Applicable | [`leader_followers.js`](../../src/Web/JavaScriptJS/patterns/leader_followers.js) | syntax + standalone execution + aggregate |
| Client-Server | Applicable | [`client_server.js`](../../src/Web/JavaScriptJS/patterns/client_server.js) | syntax + standalone execution + aggregate |
| Peer-to-Peer | Applicable | [`peer_to_peer.js`](../../src/Web/JavaScriptJS/patterns/peer_to_peer.js) | syntax + standalone execution + aggregate |
| Publish-Subscribe | Applicable | [`publish_subscribe.js`](../../src/Web/JavaScriptJS/patterns/publish_subscribe.js) | syntax + standalone execution + aggregate |
| Distributed Proxy | Applicable | [`distributed_proxy.js`](../../src/Web/JavaScriptJS/patterns/distributed_proxy.js) | syntax + standalone execution + aggregate |
| Presentation-Abstraction-Control | Applicable | [`presentation_abstraction_control.js`](../../src/Web/JavaScriptJS/patterns/presentation_abstraction_control.js) | syntax + standalone execution + aggregate |
| Model-View-Presenter | Applicable | [`model_view_presenter.js`](../../src/Web/JavaScriptJS/patterns/model_view_presenter.js) | syntax + standalone execution + aggregate |
| Document-View | Applicable | [`document_view.js`](../../src/Web/JavaScriptJS/patterns/document_view.js) | syntax + standalone execution + aggregate |
| Active Record | Applicable | [`active_record.js`](../../src/Web/JavaScriptJS/patterns/active_record.js) | syntax + standalone execution + aggregate |
| Data Mapper | Applicable | [`data_mapper.js`](../../src/Web/JavaScriptJS/patterns/data_mapper.js) | syntax + standalone execution + aggregate |
| Unit of Work | Applicable | [`unit_of_work.js`](../../src/Web/JavaScriptJS/patterns/unit_of_work.js) | syntax + standalone execution + aggregate |
| Repository | Applicable | [`repository.js`](../../src/Web/JavaScriptJS/patterns/repository.js) | syntax + standalone execution + aggregate |
| Dependency Injection | Applicable | [`dependency_injection.js`](../../src/Web/JavaScriptJS/patterns/dependency_injection.js) | syntax + standalone execution + aggregate |
| Lazy Initialization | Applicable | [`lazy_initialization.js`](../../src/Web/JavaScriptJS/patterns/lazy_initialization.js) | syntax + standalone execution + aggregate |
| Object Pool | Applicable | [`object_pool.js`](../../src/Web/JavaScriptJS/patterns/object_pool.js) | syntax + standalone execution + aggregate |
| Null Object | Applicable | [`null_object.js`](../../src/Web/JavaScriptJS/patterns/null_object.js) | syntax + standalone execution + aggregate |

## Validation contract

The target workflow resolves the current active LTS Node.js through `actions/setup-node`, prints the resolved runtime version, then for all 39 canonical files:

1. runs `node --check`;
2. executes the file standalone;
3. verifies that exactly 39 canonical files were exercised;
4. syntax-checks and executes the aggregate runner.

The aggregate must end with:

```text
javascript-pattern-sweep: 39/39 passed
```

Materialization is not verification. The reviewed head is called verified only after this gate and applicable generic repository CI are green.

## Coverage

No synthetic percentage is assigned to these standalone pedagogical examples because there is no useful homogeneous line-coverage denominator. Syntax plus independent behavioral execution and aggregate execution are the strongest lightweight evidence here; KB-006's >=44% policy remains applicable where meaningful coverage instrumentation exists.

## Cross-lane safety

This slice changes no `learn/**`, course metadata/navigation, course applications, or production architecture. JavaScript Learn remains independent from this Design Pattern matrix work.
