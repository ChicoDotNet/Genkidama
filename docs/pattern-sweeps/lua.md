# Lua Design Pattern sweep

> **State:** in-progress until the reviewed head is green and reconciled with `dev`.  
> **Scope:** 39 patterns remaining after Chain of Responsibility.  
> **Applicability:** **39 Applicable / 0 N/A**.  
> **Promotion boundary:** this ledger certifies only the Lua column; canonical pattern pages remain `in-progress` until their full KB-006 DoD is satisfied.

## Applicability

Lua can represent all 39 intents meaningfully using tables, closures, metatables where useful, first-class functions, coroutines/queues at teaching scale and explicit protocol/state boundaries. No `N/A` is introduced merely because Lua lacks nominal interfaces or textbook class syntax.

Concurrency-family examples deliberately validate coordination contracts deterministically; they do not claim scheduler throughput or race-detection evidence beyond the lightweight standalone example.

## Canonical cells

Every Applicable cell owns one individually addressable source under [`src/Scripting/Lua/patterns/`](../../src/Scripting/Lua/patterns/). [`pattern_sweep.lua`](../../src/Scripting/Lua/pattern_sweep.lua) only orchestrates those files and contains no duplicate pattern implementation.

| Pattern | Applicability | Canonical source | Validation | Idiomatic mechanism |
|---|---|---|---|---|
| Command | Applicable | [`command.lua`](../../src/Scripting/Lua/patterns/command.lua) | Lua parse + standalone execution + aggregate sweep | closure queue |
| Interpreter | Applicable | [`interpreter.lua`](../../src/Scripting/Lua/patterns/interpreter.lua) | Lua parse + standalone execution + aggregate sweep | AST table + evaluator |
| Iterator | Applicable | [`iterator.lua`](../../src/Scripting/Lua/patterns/iterator.lua) | Lua parse + standalone execution + aggregate sweep | closure iterator |
| Mediator | Applicable | [`mediator.lua`](../../src/Scripting/Lua/patterns/mediator.lua) | Lua parse + standalone execution + aggregate sweep | mediator table + callbacks |
| Memento | Applicable | [`memento.lua`](../../src/Scripting/Lua/patterns/memento.lua) | Lua parse + standalone execution + aggregate sweep | snapshot table |
| Observer | Applicable | [`observer.lua`](../../src/Scripting/Lua/patterns/observer.lua) | Lua parse + standalone execution + aggregate sweep | subscriber callbacks |
| State | Applicable | [`state.lua`](../../src/Scripting/Lua/patterns/state.lua) | Lua parse + standalone execution + aggregate sweep | state-keyed transition functions |
| Strategy | Applicable | [`strategy.lua`](../../src/Scripting/Lua/patterns/strategy.lua) | Lua parse + standalone execution + aggregate sweep | function table |
| Template Method | Applicable | [`template_method.lua`](../../src/Scripting/Lua/patterns/template_method.lua) | Lua parse + standalone execution + aggregate sweep | fixed algorithm + hook |
| Visitor | Applicable | [`visitor.lua`](../../src/Scripting/Lua/patterns/visitor.lua) | Lua parse + standalone execution + aggregate sweep | kind-dispatched function table |
| MVC | Applicable | [`mvc.lua`](../../src/Scripting/Lua/patterns/mvc.lua) | Lua parse + standalone execution + aggregate sweep | model/view/controller tables |
| MVVM | Applicable | [`mvvm.lua`](../../src/Scripting/Lua/patterns/mvvm.lua) | Lua parse + standalone execution + aggregate sweep | view-model closure boundary |
| Microkernel | Applicable | [`microkernel.lua`](../../src/Scripting/Lua/patterns/microkernel.lua) | Lua parse + standalone execution + aggregate sweep | plugin registry |
| Microservices | Applicable | [`microservices.lua`](../../src/Scripting/Lua/patterns/microservices.lua) | Lua parse + standalone execution + aggregate sweep | service functions with explicit boundary |
| Enterprise Adapter | Applicable | [`enterprise_adapter.lua`](../../src/Scripting/Lua/patterns/enterprise_adapter.lua) | Lua parse + standalone execution + aggregate sweep | shape-converting wrapper |
| Enterprise Bridge | Applicable | [`enterprise_bridge.lua`](../../src/Scripting/Lua/patterns/enterprise_bridge.lua) | Lua parse + standalone execution + aggregate sweep | abstraction + transport functions |
| Enterprise Facade | Applicable | [`enterprise_facade.lua`](../../src/Scripting/Lua/patterns/enterprise_facade.lua) | Lua parse + standalone execution + aggregate sweep | coarse-grained facade |
| Broker | Applicable | [`broker.lua`](../../src/Scripting/Lua/patterns/broker.lua) | Lua parse + standalone execution + aggregate sweep | named request router |
| Message Bus | Applicable | [`message_bus.lua`](../../src/Scripting/Lua/patterns/message_bus.lua) | Lua parse + standalone execution + aggregate sweep | topic handler registry |
| Service Locator | Applicable | [`service_locator.lua`](../../src/Scripting/Lua/patterns/service_locator.lua) | Lua parse + standalone execution + aggregate sweep | service registry |
| Active Object | Applicable | [`active_object.lua`](../../src/Scripting/Lua/patterns/active_object.lua) | Lua parse + standalone execution + aggregate sweep | queued deferred jobs |
| Monitor Object | Applicable | [`monitor_object.lua`](../../src/Scripting/Lua/patterns/monitor_object.lua) | Lua parse + standalone execution + aggregate sweep | serialized guarded access |
| Half-Sync / Half-Async | Applicable | [`half_sync_half_async.lua`](../../src/Scripting/Lua/patterns/half_sync_half_async.lua) | Lua parse + standalone execution + aggregate sweep | queue between async intake and sync worker |
| Leader / Followers | Applicable | [`leader_followers.lua`](../../src/Scripting/Lua/patterns/leader_followers.lua) | Lua parse + standalone execution + aggregate sweep | rotating leader ownership |
| Client-Server | Applicable | [`client_server.lua`](../../src/Scripting/Lua/patterns/client_server.lua) | Lua parse + standalone execution + aggregate sweep | request/response boundary |
| Peer-to-Peer | Applicable | [`peer_to_peer.lua`](../../src/Scripting/Lua/patterns/peer_to_peer.lua) | Lua parse + standalone execution + aggregate sweep | symmetric peer inboxes |
| Publish-Subscribe | Applicable | [`publish_subscribe.lua`](../../src/Scripting/Lua/patterns/publish_subscribe.lua) | Lua parse + standalone execution + aggregate sweep | topic subscriptions |
| Distributed Proxy | Applicable | [`distributed_proxy.lua`](../../src/Scripting/Lua/patterns/distributed_proxy.lua) | Lua parse + standalone execution + aggregate sweep | remote proxy + cache |
| Presentation-Abstraction-Control | Applicable | [`presentation_abstraction_control.lua`](../../src/Scripting/Lua/patterns/presentation_abstraction_control.lua) | Lua parse + standalone execution + aggregate sweep | presentation/abstraction/control triad |
| Model-View-Presenter | Applicable | [`model_view_presenter.lua`](../../src/Scripting/Lua/patterns/model_view_presenter.lua) | Lua parse + standalone execution + aggregate sweep | presenter updates passive view |
| Document-View | Applicable | [`document_view.lua`](../../src/Scripting/Lua/patterns/document_view.lua) | Lua parse + standalone execution + aggregate sweep | multiple views over one document |
| Active Record | Applicable | [`active_record.lua`](../../src/Scripting/Lua/patterns/active_record.lua) | Lua parse + standalone execution + aggregate sweep | record owns save |
| Data Mapper | Applicable | [`data_mapper.lua`](../../src/Scripting/Lua/patterns/data_mapper.lua) | Lua parse + standalone execution + aggregate sweep | mapping layer separates row/domain |
| Unit of Work | Applicable | [`unit_of_work.lua`](../../src/Scripting/Lua/patterns/unit_of_work.lua) | Lua parse + standalone execution + aggregate sweep | pending set + commit |
| Repository | Applicable | [`repository.lua`](../../src/Scripting/Lua/patterns/repository.lua) | Lua parse + standalone execution + aggregate sweep | collection-like domain access |
| Dependency Injection | Applicable | [`dependency_injection.lua`](../../src/Scripting/Lua/patterns/dependency_injection.lua) | Lua parse + standalone execution + aggregate sweep | constructor-function injection |
| Lazy Initialization | Applicable | [`lazy_initialization.lua`](../../src/Scripting/Lua/patterns/lazy_initialization.lua) | Lua parse + standalone execution + aggregate sweep | memoized creation |
| Object Pool | Applicable | [`object_pool.lua`](../../src/Scripting/Lua/patterns/object_pool.lua) | Lua parse + standalone execution + aggregate sweep | acquire/release resource pool |
| Null Object | Applicable | [`null_object.lua`](../../src/Scripting/Lua/patterns/null_object.lua) | Lua parse + standalone execution + aggregate sweep | no-op polymorphic collaborator |

## Validation

The dedicated Lua gate resolves the current stable Lua release from the official Lua distribution, verifies its published SHA-256 checksum, builds it once, runs `luac -p` over all 39 canonical sources, executes every source standalone, then runs the aggregate 39/39 orchestrator.

Code/test coverage is **N/A** for this standalone pedagogical slice: native parse + behavioral execution is stronger practical evidence than inventing a common line-coverage denominator. The repository >=44% policy remains unchanged where meaningful coverage instrumentation exists.

The workflow records `cells=39`, setup, validation and total seconds. A materialized cell is not called verified until that gate succeeds on the reviewed head.

## 80/20 boundary

This slice does not modify `learn/**`, course metadata/navigation, validators, course workflows or production architecture. Lua Learn remains owned by the course lane. The pattern examples live under the existing pattern source tree and do not advance course status.

`stable for promotion: no` until full horizontal pattern completeness is reconciled under KB-006.
