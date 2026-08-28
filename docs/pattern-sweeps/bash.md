# Bash Design Pattern matrix sweep

> **State:** materialized; verification is owned by the reviewed-head Bash gate.  
> **Scope:** 39 post-Chain-of-Responsibility patterns.  
> **Applicability:** **39 Applicable / 0 N/A**.  
> **Promotion boundary:** this ledger certifies the Bash column only; no pattern becomes `validated` from this slice alone.

## Applicability

Bash is Applicable for all 39 remaining pattern intents. The implementations use shell-native mechanisms rather than pretending Bash has nominal classes: functions as behavior boundaries, indexed/associative arrays as records and registries, explicit dispatch, background jobs/subshells where asynchronous coordination matters, and filesystem-backed locking only where a monitor boundary needs mutual exclusion.

The concurrency examples are intentionally teaching-scale. They verify observable coordination contracts; they do not claim scheduler fairness, throughput, or race-freedom beyond the behavior actually exercised.

## Canonical cells

Each Applicable cell owns an individually addressable source whose primary teaching responsibility is that pattern. [`pattern_sweep.sh`](../../src/Scripting/Bash/pattern_sweep.sh) is orchestration only.

| Family | Pattern | Applicability | Canonical source | Validation | Idiomatic mechanism |
|---|---|---|---|---|---|
| Behavioral | Command | Applicable | [`command.sh`](../../src/Scripting/Bash/patterns/command.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Behavioral | Interpreter | Applicable | [`interpreter.sh`](../../src/Scripting/Bash/patterns/interpreter.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Behavioral | Iterator | Applicable | [`iterator.sh`](../../src/Scripting/Bash/patterns/iterator.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Behavioral | Mediator | Applicable | [`mediator.sh`](../../src/Scripting/Bash/patterns/mediator.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Behavioral | Memento | Applicable | [`memento.sh`](../../src/Scripting/Bash/patterns/memento.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Behavioral | Observer | Applicable | [`observer.sh`](../../src/Scripting/Bash/patterns/observer.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Behavioral | State | Applicable | [`state.sh`](../../src/Scripting/Bash/patterns/state.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Behavioral | Strategy | Applicable | [`strategy.sh`](../../src/Scripting/Bash/patterns/strategy.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Behavioral | Template Method | Applicable | [`template_method.sh`](../../src/Scripting/Bash/patterns/template_method.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Behavioral | Visitor | Applicable | [`visitor.sh`](../../src/Scripting/Bash/patterns/visitor.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Architectural | MVC | Applicable | [`mvc.sh`](../../src/Scripting/Bash/patterns/mvc.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Architectural | MVVM | Applicable | [`mvvm.sh`](../../src/Scripting/Bash/patterns/mvvm.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Architectural | Microkernel | Applicable | [`microkernel.sh`](../../src/Scripting/Bash/patterns/microkernel.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Architectural | Microservices | Applicable | [`microservices.sh`](../../src/Scripting/Bash/patterns/microservices.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Integration | Enterprise Adapter | Applicable | [`enterprise_adapter.sh`](../../src/Scripting/Bash/patterns/enterprise_adapter.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Integration | Enterprise Bridge | Applicable | [`enterprise_bridge.sh`](../../src/Scripting/Bash/patterns/enterprise_bridge.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Integration | Enterprise Facade | Applicable | [`enterprise_facade.sh`](../../src/Scripting/Bash/patterns/enterprise_facade.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Integration | Broker | Applicable | [`broker.sh`](../../src/Scripting/Bash/patterns/broker.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Integration | Message Bus | Applicable | [`message_bus.sh`](../../src/Scripting/Bash/patterns/message_bus.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Integration | Service Locator | Applicable | [`service_locator.sh`](../../src/Scripting/Bash/patterns/service_locator.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Concurrency | Active Object | Applicable | [`active_object.sh`](../../src/Scripting/Bash/patterns/active_object.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Concurrency | Monitor Object | Applicable | [`monitor_object.sh`](../../src/Scripting/Bash/patterns/monitor_object.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Concurrency | Half-Sync / Half-Async | Applicable | [`half_sync_half_async.sh`](../../src/Scripting/Bash/patterns/half_sync_half_async.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Concurrency | Leader / Followers | Applicable | [`leader_followers.sh`](../../src/Scripting/Bash/patterns/leader_followers.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Distribution | Client-Server | Applicable | [`client_server.sh`](../../src/Scripting/Bash/patterns/client_server.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Distribution | Peer-to-Peer | Applicable | [`peer_to_peer.sh`](../../src/Scripting/Bash/patterns/peer_to_peer.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Distribution | Publish-Subscribe | Applicable | [`publish_subscribe.sh`](../../src/Scripting/Bash/patterns/publish_subscribe.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Distribution | Distributed Proxy | Applicable | [`distributed_proxy.sh`](../../src/Scripting/Bash/patterns/distributed_proxy.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Presentation | Presentation-Abstraction-Control | Applicable | [`presentation_abstraction_control.sh`](../../src/Scripting/Bash/patterns/presentation_abstraction_control.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Presentation | Model-View-Presenter | Applicable | [`model_view_presenter.sh`](../../src/Scripting/Bash/patterns/model_view_presenter.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Presentation | Document-View | Applicable | [`document_view.sh`](../../src/Scripting/Bash/patterns/document_view.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Persistence | Active Record | Applicable | [`active_record.sh`](../../src/Scripting/Bash/patterns/active_record.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Persistence | Data Mapper | Applicable | [`data_mapper.sh`](../../src/Scripting/Bash/patterns/data_mapper.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Persistence | Unit of Work | Applicable | [`unit_of_work.sh`](../../src/Scripting/Bash/patterns/unit_of_work.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Persistence | Repository | Applicable | [`repository.sh`](../../src/Scripting/Bash/patterns/repository.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Additional | Dependency Injection | Applicable | [`dependency_injection.sh`](../../src/Scripting/Bash/patterns/dependency_injection.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Additional | Lazy Initialization | Applicable | [`lazy_initialization.sh`](../../src/Scripting/Bash/patterns/lazy_initialization.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Additional | Object Pool | Applicable | [`object_pool.sh`](../../src/Scripting/Bash/patterns/object_pool.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |
| Additional | Null Object | Applicable | [`null_object.sh`](../../src/Scripting/Bash/patterns/null_object.sh) | `bash -n` + standalone execution | Bash functions/data/dispatch preserve the intent |

## Validation

The target workflow builds GNU Bash 5.3 from the official GNU release tarball, verifies SHA-256 `62dd49c44c399ed1b3f7f731e87a782334d834f08e098a35f2c87547d5dbb269`, reports the resolved runtime, then:

1. runs `bash -n` on all 39 canonical cells plus the runner;
2. executes every canonical cell standalone under the built runtime;
3. executes the aggregate runner and requires `bash-pattern-sweep: 39/39 passed`;
4. records `cells`, setup, validation and total seconds.

Materialization is not called verified until that gate is green on the reviewed head.

## Coverage

No synthetic line-coverage percentage is assigned. These standalone shell teaching artifacts are better certified by parser validation plus independent behavioral execution and aggregate execution. The repository's >=44% coverage floor remains unchanged wherever meaningful percentage tooling exists.

## Cross-lane boundary

This sweep does not change `learn/**`, course metadata/navigation/validators, course workflows, or production architecture. Bash Learn remains owned by the course lane. The Design Pattern catalog remains architecture-first.
