# Chapel Design Pattern matrix sweep

> **Target:** Chapel  
> **Language target position:** #53  
> **Pattern universe:** 52  
> **Applicability:** 52 Applicable / 0 N/A  
> **Toolchain:** Chapel 2.10.0 on Ubuntu 24.04  
> **Delivery:** contract-first RED → implementation GREEN → reconciliation

## Evidence

The RED head installed Chapel 2.10.0 successfully and failed all 52 cells for the intended reason: every contracted test existed while every canonical source was still absent.

The first implementation head compiled and executed **51/52** cells successfully. Bridge was the only behavioral mismatch: its implementation emitted `tv:muted` while the already-fixed test contract required `tv=muted`. The reconciliation head changes only that behavior and delegates historical pre-CoR checks to this full target gate rather than re-imposing legacy textual markers.

The authoritative final result is the exact-head `Chapel / 2.10.0` Polyglot job. This ledger intentionally does not hard-code a future workflow run number.

## Full 52-cell ledger

Every cell owns an individually addressable canonical source and test.

| Family | Pattern | Canonical source | Contract |
|---|---|---|---|
| Creational | Abstract Factory | [`abstract_factory.chpl`](../../src/Systems/Chapel/patterns/abstract_factory.chpl) | [test](../../src/Systems/Chapel/tests/test_abstract_factory.chpl) |
| Creational | Builder | [`builder.chpl`](../../src/Systems/Chapel/patterns/builder.chpl) | [test](../../src/Systems/Chapel/tests/test_builder.chpl) |
| Creational | Factory Method | [`factory_method.chpl`](../../src/Systems/Chapel/patterns/factory_method.chpl) | [test](../../src/Systems/Chapel/tests/test_factory_method.chpl) |
| Creational | Prototype | [`prototype.chpl`](../../src/Systems/Chapel/patterns/prototype.chpl) | [test](../../src/Systems/Chapel/tests/test_prototype.chpl) |
| Creational | Singleton | [`singleton.chpl`](../../src/Systems/Chapel/patterns/singleton.chpl) | [test](../../src/Systems/Chapel/tests/test_singleton.chpl) |
| Structural | Adapter | [`adapter.chpl`](../../src/Systems/Chapel/patterns/adapter.chpl) | [test](../../src/Systems/Chapel/tests/test_adapter.chpl) |
| Structural | Bridge | [`bridge.chpl`](../../src/Systems/Chapel/patterns/bridge.chpl) | [test](../../src/Systems/Chapel/tests/test_bridge.chpl) |
| Structural | Composite | [`composite.chpl`](../../src/Systems/Chapel/patterns/composite.chpl) | [test](../../src/Systems/Chapel/tests/test_composite.chpl) |
| Structural | Decorator | [`decorator.chpl`](../../src/Systems/Chapel/patterns/decorator.chpl) | [test](../../src/Systems/Chapel/tests/test_decorator.chpl) |
| Structural | Facade | [`facade.chpl`](../../src/Systems/Chapel/patterns/facade.chpl) | [test](../../src/Systems/Chapel/tests/test_facade.chpl) |
| Structural | Flyweight | [`flyweight.chpl`](../../src/Systems/Chapel/patterns/flyweight.chpl) | [test](../../src/Systems/Chapel/tests/test_flyweight.chpl) |
| Structural | Proxy | [`proxy.chpl`](../../src/Systems/Chapel/patterns/proxy.chpl) | [test](../../src/Systems/Chapel/tests/test_proxy.chpl) |
| Behavioral | Chain of Responsibility | [`chain_of_responsibility.chpl`](../../src/Systems/Chapel/patterns/chain_of_responsibility.chpl) | [test](../../src/Systems/Chapel/tests/test_chain_of_responsibility.chpl) |
| Behavioral | Command | [`command.chpl`](../../src/Systems/Chapel/patterns/command.chpl) | [test](../../src/Systems/Chapel/tests/test_command.chpl) |
| Behavioral | Interpreter | [`interpreter.chpl`](../../src/Systems/Chapel/patterns/interpreter.chpl) | [test](../../src/Systems/Chapel/tests/test_interpreter.chpl) |
| Behavioral | Iterator | [`iterator.chpl`](../../src/Systems/Chapel/patterns/iterator.chpl) | [test](../../src/Systems/Chapel/tests/test_iterator.chpl) |
| Behavioral | Mediator | [`mediator.chpl`](../../src/Systems/Chapel/patterns/mediator.chpl) | [test](../../src/Systems/Chapel/tests/test_mediator.chpl) |
| Behavioral | Memento | [`memento.chpl`](../../src/Systems/Chapel/patterns/memento.chpl) | [test](../../src/Systems/Chapel/tests/test_memento.chpl) |
| Behavioral | Observer | [`observer.chpl`](../../src/Systems/Chapel/patterns/observer.chpl) | [test](../../src/Systems/Chapel/tests/test_observer.chpl) |
| Behavioral | State | [`state.chpl`](../../src/Systems/Chapel/patterns/state.chpl) | [test](../../src/Systems/Chapel/tests/test_state.chpl) |
| Behavioral | Strategy | [`strategy.chpl`](../../src/Systems/Chapel/patterns/strategy.chpl) | [test](../../src/Systems/Chapel/tests/test_strategy.chpl) |
| Behavioral | Template Method | [`template_method.chpl`](../../src/Systems/Chapel/patterns/template_method.chpl) | [test](../../src/Systems/Chapel/tests/test_template_method.chpl) |
| Behavioral | Visitor | [`visitor.chpl`](../../src/Systems/Chapel/patterns/visitor.chpl) | [test](../../src/Systems/Chapel/tests/test_visitor.chpl) |
| Architectural | MVC | [`mvc.chpl`](../../src/Systems/Chapel/patterns/mvc.chpl) | [test](../../src/Systems/Chapel/tests/test_mvc.chpl) |
| Architectural | MVVM | [`mvvm.chpl`](../../src/Systems/Chapel/patterns/mvvm.chpl) | [test](../../src/Systems/Chapel/tests/test_mvvm.chpl) |
| Architectural | Microkernel | [`microkernel.chpl`](../../src/Systems/Chapel/patterns/microkernel.chpl) | [test](../../src/Systems/Chapel/tests/test_microkernel.chpl) |
| Architectural | Microservices | [`microservices.chpl`](../../src/Systems/Chapel/patterns/microservices.chpl) | [test](../../src/Systems/Chapel/tests/test_microservices.chpl) |
| Integration | Enterprise Adapter | [`enterprise_adapter.chpl`](../../src/Systems/Chapel/patterns/enterprise_adapter.chpl) | [test](../../src/Systems/Chapel/tests/test_enterprise_adapter.chpl) |
| Integration | Enterprise Bridge | [`enterprise_bridge.chpl`](../../src/Systems/Chapel/patterns/enterprise_bridge.chpl) | [test](../../src/Systems/Chapel/tests/test_enterprise_bridge.chpl) |
| Integration | Enterprise Facade | [`enterprise_facade.chpl`](../../src/Systems/Chapel/patterns/enterprise_facade.chpl) | [test](../../src/Systems/Chapel/tests/test_enterprise_facade.chpl) |
| Integration | Broker | [`broker.chpl`](../../src/Systems/Chapel/patterns/broker.chpl) | [test](../../src/Systems/Chapel/tests/test_broker.chpl) |
| Integration | Message Bus | [`message_bus.chpl`](../../src/Systems/Chapel/patterns/message_bus.chpl) | [test](../../src/Systems/Chapel/tests/test_message_bus.chpl) |
| Integration | Service Locator | [`service_locator.chpl`](../../src/Systems/Chapel/patterns/service_locator.chpl) | [test](../../src/Systems/Chapel/tests/test_service_locator.chpl) |
| Concurrency | Active Object | [`active_object.chpl`](../../src/Systems/Chapel/patterns/active_object.chpl) | [test](../../src/Systems/Chapel/tests/test_active_object.chpl) |
| Concurrency | Monitor Object | [`monitor_object.chpl`](../../src/Systems/Chapel/patterns/monitor_object.chpl) | [test](../../src/Systems/Chapel/tests/test_monitor_object.chpl) |
| Concurrency | Half-Sync / Half-Async | [`half_sync_half_async.chpl`](../../src/Systems/Chapel/patterns/half_sync_half_async.chpl) | [test](../../src/Systems/Chapel/tests/test_half_sync_half_async.chpl) |
| Concurrency | Leader / Followers | [`leader_followers.chpl`](../../src/Systems/Chapel/patterns/leader_followers.chpl) | [test](../../src/Systems/Chapel/tests/test_leader_followers.chpl) |
| Distribution | Client-Server | [`client_server.chpl`](../../src/Systems/Chapel/patterns/client_server.chpl) | [test](../../src/Systems/Chapel/tests/test_client_server.chpl) |
| Distribution | Peer-to-Peer | [`peer_to_peer.chpl`](../../src/Systems/Chapel/patterns/peer_to_peer.chpl) | [test](../../src/Systems/Chapel/tests/test_peer_to_peer.chpl) |
| Distribution | Publish-Subscribe | [`publish_subscribe.chpl`](../../src/Systems/Chapel/patterns/publish_subscribe.chpl) | [test](../../src/Systems/Chapel/tests/test_publish_subscribe.chpl) |
| Distribution | Distributed Proxy | [`distributed_proxy.chpl`](../../src/Systems/Chapel/patterns/distributed_proxy.chpl) | [test](../../src/Systems/Chapel/tests/test_distributed_proxy.chpl) |
| Presentation | Presentation-Abstraction-Control | [`presentation_abstraction_control.chpl`](../../src/Systems/Chapel/patterns/presentation_abstraction_control.chpl) | [test](../../src/Systems/Chapel/tests/test_presentation_abstraction_control.chpl) |
| Presentation | Model-View-Presenter | [`model_view_presenter.chpl`](../../src/Systems/Chapel/patterns/model_view_presenter.chpl) | [test](../../src/Systems/Chapel/tests/test_model_view_presenter.chpl) |
| Presentation | Document-View | [`document_view.chpl`](../../src/Systems/Chapel/patterns/document_view.chpl) | [test](../../src/Systems/Chapel/tests/test_document_view.chpl) |
| Persistence | Active Record | [`active_record.chpl`](../../src/Systems/Chapel/patterns/active_record.chpl) | [test](../../src/Systems/Chapel/tests/test_active_record.chpl) |
| Persistence | Data Mapper | [`data_mapper.chpl`](../../src/Systems/Chapel/patterns/data_mapper.chpl) | [test](../../src/Systems/Chapel/tests/test_data_mapper.chpl) |
| Persistence | Unit of Work | [`unit_of_work.chpl`](../../src/Systems/Chapel/patterns/unit_of_work.chpl) | [test](../../src/Systems/Chapel/tests/test_unit_of_work.chpl) |
| Persistence | Repository | [`repository.chpl`](../../src/Systems/Chapel/patterns/repository.chpl) | [test](../../src/Systems/Chapel/tests/test_repository.chpl) |
| Additional | Dependency Injection | [`dependency_injection.chpl`](../../src/Systems/Chapel/patterns/dependency_injection.chpl) | [test](../../src/Systems/Chapel/tests/test_dependency_injection.chpl) |
| Additional | Lazy Initialization | [`lazy_initialization.chpl`](../../src/Systems/Chapel/patterns/lazy_initialization.chpl) | [test](../../src/Systems/Chapel/tests/test_lazy_initialization.chpl) |
| Additional | Object Pool | [`object_pool.chpl`](../../src/Systems/Chapel/patterns/object_pool.chpl) | [test](../../src/Systems/Chapel/tests/test_object_pool.chpl) |
| Additional | Null Object | [`null_object.chpl`](../../src/Systems/Chapel/patterns/null_object.chpl) | [test](../../src/Systems/Chapel/tests/test_null_object.chpl) |

## Documentation boundary

Chapel is a normal language target, so the 21 already-authored pattern pages include its row and move their current language denominator from 52 to 53. The 31 pre-existing empty pattern pages remain KB-006 authoring debt; this ledger provides the Chapel cell evidence without fabricating missing prose.
