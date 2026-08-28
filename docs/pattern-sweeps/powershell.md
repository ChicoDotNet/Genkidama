# PowerShell pattern sweep

Status: experimental language-major slice under the roadmap exception. This ledger does not promote any canonical pattern by itself.

## Scope

- Target: PowerShell
- Canonical cells: `src/Scripting/PowerShell/patterns/*.ps1`
- Orchestrator: `src/Scripting/PowerShell/pattern_sweep.ps1`
- Applicability: **39 Applicable / 0 N/A**
- Canonicality: every `pattern × PowerShell` cell has its own addressable source; the runner only orchestrates them.
- Canonical pattern status remains unchanged until each pattern satisfies the full horizontal KB-006 DoD.

## Matrix

| Pattern | Applicability | Canonical artifact | Validation |
| --- | --- | --- | --- |
| Command | Applicable | [`command.ps1`](../../src/Scripting/PowerShell/patterns/command.ps1) | parser + standalone execution |
| Interpreter | Applicable | [`interpreter.ps1`](../../src/Scripting/PowerShell/patterns/interpreter.ps1) | parser + standalone execution |
| Iterator | Applicable | [`iterator.ps1`](../../src/Scripting/PowerShell/patterns/iterator.ps1) | parser + standalone execution |
| Mediator | Applicable | [`mediator.ps1`](../../src/Scripting/PowerShell/patterns/mediator.ps1) | parser + standalone execution |
| Memento | Applicable | [`memento.ps1`](../../src/Scripting/PowerShell/patterns/memento.ps1) | parser + standalone execution |
| Observer | Applicable | [`observer.ps1`](../../src/Scripting/PowerShell/patterns/observer.ps1) | parser + standalone execution |
| State | Applicable | [`state.ps1`](../../src/Scripting/PowerShell/patterns/state.ps1) | parser + standalone execution |
| Strategy | Applicable | [`strategy.ps1`](../../src/Scripting/PowerShell/patterns/strategy.ps1) | parser + standalone execution |
| Template Method | Applicable | [`template-method.ps1`](../../src/Scripting/PowerShell/patterns/template-method.ps1) | parser + standalone execution |
| Visitor | Applicable | [`visitor.ps1`](../../src/Scripting/PowerShell/patterns/visitor.ps1) | parser + standalone execution |
| MVC | Applicable | [`mvc.ps1`](../../src/Scripting/PowerShell/patterns/mvc.ps1) | parser + standalone execution |
| MVVM | Applicable | [`mvvm.ps1`](../../src/Scripting/PowerShell/patterns/mvvm.ps1) | parser + standalone execution |
| Microkernel | Applicable | [`microkernel.ps1`](../../src/Scripting/PowerShell/patterns/microkernel.ps1) | parser + standalone execution |
| Microservices | Applicable | [`microservices.ps1`](../../src/Scripting/PowerShell/patterns/microservices.ps1) | parser + standalone execution |
| Enterprise Adapter | Applicable | [`enterprise-adapter.ps1`](../../src/Scripting/PowerShell/patterns/enterprise-adapter.ps1) | parser + standalone execution |
| Enterprise Bridge | Applicable | [`enterprise-bridge.ps1`](../../src/Scripting/PowerShell/patterns/enterprise-bridge.ps1) | parser + standalone execution |
| Enterprise Facade | Applicable | [`enterprise-facade.ps1`](../../src/Scripting/PowerShell/patterns/enterprise-facade.ps1) | parser + standalone execution |
| Broker | Applicable | [`broker.ps1`](../../src/Scripting/PowerShell/patterns/broker.ps1) | parser + standalone execution |
| Message Bus | Applicable | [`message-bus.ps1`](../../src/Scripting/PowerShell/patterns/message-bus.ps1) | parser + standalone execution |
| Service Locator | Applicable | [`service-locator.ps1`](../../src/Scripting/PowerShell/patterns/service-locator.ps1) | parser + standalone execution |
| Active Object | Applicable | [`active-object.ps1`](../../src/Scripting/PowerShell/patterns/active-object.ps1) | parser + standalone execution |
| Monitor Object | Applicable | [`monitor-object.ps1`](../../src/Scripting/PowerShell/patterns/monitor-object.ps1) | parser + standalone execution |
| Half-Sync / Half-Async | Applicable | [`half-sync-half-async.ps1`](../../src/Scripting/PowerShell/patterns/half-sync-half-async.ps1) | parser + standalone execution |
| Leader / Followers | Applicable | [`leader-followers.ps1`](../../src/Scripting/PowerShell/patterns/leader-followers.ps1) | parser + standalone execution |
| Client-Server | Applicable | [`client-server.ps1`](../../src/Scripting/PowerShell/patterns/client-server.ps1) | parser + standalone execution |
| Peer-to-Peer | Applicable | [`peer-to-peer.ps1`](../../src/Scripting/PowerShell/patterns/peer-to-peer.ps1) | parser + standalone execution |
| Publish-Subscribe | Applicable | [`publish-subscribe.ps1`](../../src/Scripting/PowerShell/patterns/publish-subscribe.ps1) | parser + standalone execution |
| Distributed Proxy | Applicable | [`distributed-proxy.ps1`](../../src/Scripting/PowerShell/patterns/distributed-proxy.ps1) | parser + standalone execution |
| Presentation-Abstraction-Control | Applicable | [`presentation-abstraction-control.ps1`](../../src/Scripting/PowerShell/patterns/presentation-abstraction-control.ps1) | parser + standalone execution |
| Model-View-Presenter | Applicable | [`model-view-presenter.ps1`](../../src/Scripting/PowerShell/patterns/model-view-presenter.ps1) | parser + standalone execution |
| Document-View | Applicable | [`document-view.ps1`](../../src/Scripting/PowerShell/patterns/document-view.ps1) | parser + standalone execution |
| Active Record | Applicable | [`active-record.ps1`](../../src/Scripting/PowerShell/patterns/active-record.ps1) | parser + standalone execution |
| Data Mapper | Applicable | [`data-mapper.ps1`](../../src/Scripting/PowerShell/patterns/data-mapper.ps1) | parser + standalone execution |
| Unit of Work | Applicable | [`unit-of-work.ps1`](../../src/Scripting/PowerShell/patterns/unit-of-work.ps1) | parser + standalone execution |
| Repository | Applicable | [`repository.ps1`](../../src/Scripting/PowerShell/patterns/repository.ps1) | parser + standalone execution |
| Dependency Injection | Applicable | [`dependency-injection.ps1`](../../src/Scripting/PowerShell/patterns/dependency-injection.ps1) | parser + standalone execution |
| Lazy Initialization | Applicable | [`lazy-initialization.ps1`](../../src/Scripting/PowerShell/patterns/lazy-initialization.ps1) | parser + standalone execution |
| Object Pool | Applicable | [`object-pool.ps1`](../../src/Scripting/PowerShell/patterns/object-pool.ps1) | parser + standalone execution |
| Null Object | Applicable | [`null-object.ps1`](../../src/Scripting/PowerShell/patterns/null-object.ps1) | parser + standalone execution |

## Verification

The dedicated workflow parses every canonical script with PowerShell's parser, executes every script standalone, then executes the aggregate runner. A successful runner emits:

`powershell-pattern-sweep: 39/39 passed`

CI runtime version and timing evidence are intentionally not recorded here until observed on a reviewed head.

## Coverage

Percentage line coverage is not reasonably informative for these standalone pedagogical scripts. Validation therefore uses the strongest lightweight evidence available: parser diagnostics plus behavioral assertions in each canonical cell, independent execution, and aggregate execution. No synthetic percentage is assigned.
