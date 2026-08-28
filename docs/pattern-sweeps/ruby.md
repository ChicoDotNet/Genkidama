# Ruby pattern sweep

> **Scope:** Ruby column for the 39 catalog patterns remaining after Chain of Responsibility.  
> **Status:** in progress until the Ruby target gate passes on the reviewed head.  
> **Canonical completion:** unchanged; each pattern remains incomplete until all Applicable languages satisfy KB-006.

## Applicability

Ruby can express all 39 remaining pattern intents meaningfully through objects, modules, blocks, lambdas, Enumerators, duck typing, message passing, threads/queues and standard synchronization primitives. Therefore every cell in this slice is classified `Applicable`; there are no Ruby `N/A` classifications.

## Implementation

All cells are implemented in [`src/Scripting/Ruby/pattern_sweep.rb`](../../src/Scripting/Ruby/pattern_sweep.rb). The harness isolates the examples from Genkidama production and Learn code and executes one observable contract per pattern.

| Pattern | Applicability | Implementation | Validation |
|---|---|---|---|
| Command | Applicable | `PatternSweep.command` | Ruby syntax + execution |
| Interpreter | Applicable | `PatternSweep.interpreter` | Ruby syntax + execution |
| Iterator | Applicable | `PatternSweep.iterator` | Ruby syntax + execution |
| Mediator | Applicable | `PatternSweep.mediator` | Ruby syntax + execution |
| Memento | Applicable | `PatternSweep.memento` | Ruby syntax + execution |
| Observer | Applicable | `PatternSweep.observer` | Ruby syntax + execution |
| State | Applicable | `PatternSweep.state` | Ruby syntax + execution |
| Strategy | Applicable | `PatternSweep.strategy` | Ruby syntax + execution |
| Template Method | Applicable | `PatternSweep.template_method` | Ruby syntax + execution |
| Visitor | Applicable | `PatternSweep.visitor` | Ruby syntax + execution |
| MVC | Applicable | `PatternSweep.mvc` | Ruby syntax + execution |
| MVVM | Applicable | `PatternSweep.mvvm` | Ruby syntax + execution |
| Microkernel | Applicable | `PatternSweep.microkernel` | Ruby syntax + execution |
| Microservices | Applicable | `PatternSweep.microservices` | Ruby syntax + execution |
| Enterprise Adapter | Applicable | `PatternSweep.enterprise_adapter` | Ruby syntax + execution |
| Enterprise Bridge | Applicable | `PatternSweep.enterprise_bridge` | Ruby syntax + execution |
| Enterprise Facade | Applicable | `PatternSweep.enterprise_facade` | Ruby syntax + execution |
| Broker | Applicable | `PatternSweep.broker` | Ruby syntax + execution |
| Message Bus | Applicable | `PatternSweep.message_bus` | Ruby syntax + execution |
| Service Locator | Applicable | `PatternSweep.service_locator` | Ruby syntax + execution |
| Active Object | Applicable | `PatternSweep.active_object` | Ruby syntax + execution |
| Monitor Object | Applicable | `PatternSweep.monitor_object` | Ruby syntax + execution |
| Half-Sync / Half-Async | Applicable | `PatternSweep.half_sync_half_async` | Ruby syntax + execution |
| Leader / Followers | Applicable | `PatternSweep.leader_followers` | Ruby syntax + execution |
| Client-Server | Applicable | `PatternSweep.client_server` | Ruby syntax + execution |
| Peer-to-Peer | Applicable | `PatternSweep.peer_to_peer` | Ruby syntax + execution |
| Publish-Subscribe | Applicable | `PatternSweep.publish_subscribe` | Ruby syntax + execution |
| Distributed Proxy | Applicable | `PatternSweep.distributed_proxy` | Ruby syntax + execution |
| Presentation-Abstraction-Control | Applicable | `PatternSweep.presentation_abstraction_control` | Ruby syntax + execution |
| Model-View-Presenter | Applicable | `PatternSweep.model_view_presenter` | Ruby syntax + execution |
| Document-View | Applicable | `PatternSweep.document_view` | Ruby syntax + execution |
| Active Record | Applicable | `PatternSweep.active_record` | Ruby syntax + execution |
| Data Mapper | Applicable | `PatternSweep.data_mapper` | Ruby syntax + execution |
| Unit of Work | Applicable | `PatternSweep.unit_of_work` | Ruby syntax + execution |
| Repository | Applicable | `PatternSweep.repository` | Ruby syntax + execution |
| Dependency Injection | Applicable | `PatternSweep.dependency_injection` | Ruby syntax + execution |
| Lazy Initialization | Applicable | `PatternSweep.lazy_initialization` | Ruby syntax + execution |
| Object Pool | Applicable | `PatternSweep.object_pool` | Ruby syntax + execution |
| Null Object | Applicable | `PatternSweep.null_object` | Ruby syntax + execution |

## Validation contract

The target workflow runs `ruby -c` and then executes the entire 39-cell harness using Ruby 3.3. A successful run must print:

```text
ruby-pattern-sweep: 39/39 passed
```

The workflow also emits `cells`, `setup_seconds`, `validation_seconds` and `total_seconds` so later batching decisions use observed CI cost rather than assumptions.

## Coverage

Percentage code/test coverage is not assigned to this pedagogical standalone harness because there is no useful homogeneous line-coverage denominator for the catalog matrix. The gate instead validates syntax and the promised behavior of every Ruby cell. This follows KB-006: use the strongest lightweight validation reasonably available and do not invent percentages.

## Cross-lane safety

This slice does not modify `learn/**`, course metadata/navigation, course applications, or production architecture. Ruby Learn remains an independently completed course; this file only tracks Design Pattern matrix evidence.
