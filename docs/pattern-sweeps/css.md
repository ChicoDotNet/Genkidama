# CSS Design Pattern matrix sweep

> **State:** applicability classified; no executable cells exist because pure CSS is `N/A` for all 39 remaining patterns.  
> **Scope:** CSS target × 39 patterns remaining after Chain of Responsibility.  
> **Applicability:** **0 Applicable / 39 N/A**.  
> **Promotion boundary:** this ledger closes only the CSS applicability row. It does not mark any canonical pattern page `validated`.

## Target boundary

This target means **CSS itself**, not JavaScript, browser scripting, component frameworks, preprocessors with executable host logic, CSS-in-JS, build tooling or an application that happens to emit stylesheets.

CSS is a declarative styling language evaluated by the browser's style engine. It has selectors, cascade, inheritance, custom properties, conditions and limited state-sensitive matching, but it does not expose a general user-programmable execution model for arbitrary functions, dispatch, messages, object/process lifecycle, transactions, persistence, dependency provisioning or application-level synchronization.

KB-006 explicitly forbids using “no classes/OOP” as a shortcut to `N/A`; that is not the rationale here. The classification is based on whether the **pattern intent itself** can be represented meaningfully in CSS as the target runtime. Styling composition or selector precedence can resemble structural ideas, but resemblance is not enough when the pattern requires executable collaboration, lifecycle or protocol semantics that CSS cannot own.

This boundary is consistent with the already-reviewed Command page on `dev`, which classifies CSS `N/A` because declarative style rules do not provide a general request/invoker/receiver execution model.

## Applicability decisions

| Pattern | Applicability | Technical justification |
|---|---|---|
| Command | N/A | CSS rules cannot reify, queue or dispatch an application request to a receiver; browser style evaluation is not a programmable command dispatcher. |
| Interpreter | N/A | CSS itself is interpreted by the browser, but authors cannot use CSS to define and execute an interpreter for another language or grammar. |
| Iterator | N/A | Selectors match sets of elements, but CSS exposes no programmable traversal cursor, next/current protocol or iteration state controlled by the stylesheet. |
| Mediator | N/A | Shared selectors can influence many elements, but CSS cannot own executable peer coordination or route collaboration among components. |
| Memento | N/A | Custom properties and declarations can encode values, but CSS cannot capture and later restore application object state through originator/caretaker semantics. |
| Observer | N/A | CSS reacts to browser-computed states such as `:hover` or media queries, but authors cannot define arbitrary subscription/notification relationships in CSS itself. |
| State | N/A | Selectors can style externally represented states, but CSS cannot encapsulate application state transitions and vary executable behavior through state objects/values. |
| Strategy | N/A | Alternative rule sets can change presentation, but CSS has no runtime algorithm interface or caller-controlled selection among interchangeable executable strategies. |
| Template Method | N/A | Cascade/inheritance can share declarations, but CSS has no executable invariant algorithm with overridable procedural steps. |
| Visitor | N/A | Selectors target heterogeneous nodes, but CSS cannot define a programmable operation with visitor-style dispatch over element types. |
| MVC | N/A | CSS can style a View, but it cannot implement Model, Controller or the executable collaboration among MVC participants. |
| MVVM | N/A | CSS can style rendered bindings, but it has no ViewModel behavior, binding engine or command/update semantics of its own. |
| Microkernel | N/A | CSS has no executable core, plugin lifecycle, discovery contract or runtime extension boundary under application control. |
| Microservices | N/A | CSS has no independently deployable service process, service contract or inter-service communication semantics. |
| Enterprise Adapter | N/A | CSS can normalize visual presentation but cannot execute protocol/data translation between incompatible enterprise interfaces. |
| Enterprise Bridge | N/A | CSS composition cannot independently vary an executable abstraction and implementation through delegation. |
| Enterprise Facade | N/A | A stylesheet may simplify presentation conventions, but it cannot expose an executable subsystem-facing facade API. |
| Broker | N/A | CSS has no participant registry, invocation routing, marshaling or message-delivery execution model. |
| Message Bus | N/A | CSS cannot publish, route or consume application messages; cascade propagation is style resolution, not messaging. |
| Service Locator | N/A | CSS custom properties are values in style resolution, not a runtime registry that locates executable services for consumers. |
| Active Object | N/A | CSS has no object-owned scheduler, request queue or asynchronous method execution mechanism. |
| Monitor Object | N/A | CSS exposes neither programmable shared mutable application state nor locks/condition synchronization. |
| Half-Sync / Half-Async | N/A | CSS cannot establish synchronous and asynchronous execution layers or queue work between them. |
| Leader / Followers | N/A | CSS has no worker pool, event-source ownership or leader handoff protocol. |
| Client-Server | N/A | Stylesheets may be transferred over client-server protocols, but CSS itself implements neither client nor server process behavior. |
| Peer-to-Peer | N/A | CSS cannot discover peers, exchange application messages or execute as a network peer. |
| Publish-Subscribe | N/A | Media/state selectors react to environment, but CSS cannot define publishers, subscriptions and fan-out delivery of application events. |
| Distributed Proxy | N/A | `url()` can reference remote resources, but CSS cannot implement proxy identity, marshaling, remote invocation or failure behavior. |
| Presentation-Abstraction-Control | N/A | CSS contributes presentation only; it cannot implement PAC agents or their control/abstraction communication. |
| Model-View-Presenter | N/A | CSS can style a View, but cannot implement Presenter behavior or the MVP interaction contract. |
| Document-View | N/A | CSS can style document views, but cannot coordinate an editable document model with multiple programmable view objects. |
| Active Record | N/A | CSS has no storage identity, query, load/save or row-oriented persistence behavior. |
| Data Mapper | N/A | CSS cannot execute mapping between domain objects and a persistence representation. |
| Unit of Work | N/A | CSS has no change tracking, transaction boundary, commit or rollback semantics. |
| Repository | N/A | CSS cannot expose a collection-like persistence boundary with executable query/save behavior. |
| Dependency Injection | N/A | Cascade, inheritance and custom properties provide declarative value propagation, not runtime provisioning of executable dependencies to consumers. |
| Lazy Initialization | N/A | Browser stylesheet/resource loading behavior is platform-managed; CSS cannot own application-object creation-on-first-use semantics. |
| Object Pool | N/A | CSS has no programmable object lifecycle, allocation, checkout/return protocol or reusable-instance pool. |
| Null Object | N/A | Fallback declarations or empty style effects are presentation defaults, not substitutable executable collaborators implementing neutral behavior. |

## Validation boundary

There are no canonical source artifacts to compile or run because the row contains **zero Applicable cells**. Validation for this slice is factual and structural:

- the target boundary is explicitly pure CSS rather than JavaScript/framework/build-tool behavior;
- every one of the 39 cells has an intent-level technical justification;
- no `N/A` relies merely on absence of classes or textbook OOP syntax;
- selector/cascade analogies are not promoted to pattern implementations when required executable semantics are absent;
- no fake implementation path or executable evidence is claimed;
- the existing Command classification on `dev` is preserved rather than contradicted.

No code/test coverage percentage is meaningful for a zero-executable-cell applicability row; coverage is **N/A** for this slice.

## Cross-lane compatibility

This ledger does not modify `learn/**`, course metadata, navigation, validators, course workflows or production architecture. CSS is a Design Pattern target in the 51-target matrix but is not one of the 45 standalone Learn course entries; this applicability work does not alter the course denominator or course backlog.

## Reconciliation rule

At horizontal closure time, each canonical pattern page must reconcile its CSS row to `N/A` with an equivalent technical reason. This ledger does not by itself change a pattern's `in-progress` status or satisfy the remaining Applicable-language cells.
