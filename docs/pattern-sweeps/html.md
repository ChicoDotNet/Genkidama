# HTML Design Pattern matrix sweep

> **State:** applicability classified; no executable cells exist because pure HTML is `N/A` for all 39 remaining patterns.  
> **Scope:** HTML target × 39 patterns remaining after Chain of Responsibility.  
> **Applicability:** **0 Applicable / 39 N/A**.  
> **Promotion boundary:** this ledger closes only the HTML applicability row. It does not mark any canonical pattern page `validated`.

## Target boundary

This target means **HTML itself**, not JavaScript, Web Components, a browser framework, server-side rendering code, CSS, templating languages, DOM scripting or a web application that happens to emit HTML.

HTML is a declarative document-markup language. It can describe document structure, hyperlinks, forms and semantic content consumed by a user agent, but it does not provide an application execution model for user-defined functions, dispatch, mutable application state, processes, messages, transactions, dependency wiring, persistence coordination or runtime substitution.

KB-006 explicitly forbids using “no classes/OOP” as a shortcut to `N/A`; that is not the rationale here. The classification is based on the **absence of a programmable execution mechanism in the target itself**. When the pattern intent can be implemented by JavaScript, a framework, a server process or another language around the markup, that implementation belongs to that executable target rather than HTML.

This boundary is consistent with the already-reviewed Command page on `dev`, which classifies HTML `N/A` because markup has no execution model capable of encapsulating and dispatching an action as a value.

## Applicability decisions

| Pattern | Applicability | Technical justification |
|---|---|---|
| Command | N/A | HTML can describe controls that may trigger browser/application behavior, but cannot reify, queue or dispatch an executable request itself. |
| Interpreter | N/A | HTML can represent syntax trees as markup but cannot define or execute an interpreter/evaluator over a language. |
| Iterator | N/A | Document order exists, but HTML cannot define programmable traversal state or a next/current iteration protocol. |
| Mediator | N/A | Elements may participate in browser-managed interaction, but HTML cannot own executable coordination logic between peers. |
| Memento | N/A | Markup can serialize a document snapshot, but HTML has no originator/caretaker behavior or restoration semantics for application state. |
| Observer | N/A | HTML exposes no user-defined subscription/notification mechanism; DOM events require an executable scripting/runtime target. |
| State | N/A | Static markup can encode a state label, but cannot vary runtime behavior by encapsulated state transitions. |
| Strategy | N/A | HTML cannot encapsulate interchangeable executable algorithms or select among them at runtime. |
| Template Method | N/A | Markup templates are not Template Method: HTML has no executable invariant algorithm with overridable steps. |
| Visitor | N/A | HTML can form a tree, but cannot define double-dispatch or another programmable operation visiting heterogeneous nodes. |
| MVC | N/A | HTML can serve as a rendered View artifact, but cannot implement the Model-View-Controller collaboration by itself. |
| MVVM | N/A | HTML can host rendered bindings supplied by another runtime, but has no view-model behavior or binding engine of its own. |
| Microkernel | N/A | HTML has no executable core, plugin contract, discovery mechanism or runtime extension boundary. |
| Microservices | N/A | A document can link to services, but cannot implement independently deployable service processes or their interaction. |
| Enterprise Adapter | N/A | HTML can describe input/output structure but cannot execute protocol/data adaptation logic. |
| Enterprise Bridge | N/A | HTML cannot vary an abstraction and implementation independently through executable delegation. |
| Enterprise Facade | N/A | HTML can present a simplified UI, but cannot expose an executable subsystem-facing facade contract itself. |
| Broker | N/A | HTML has no routing, registration, invocation or message-delivery execution model. |
| Message Bus | N/A | HTML cannot publish, route or consume application messages; hyperlinks/forms are browser navigation primitives, not a programmable message bus. |
| Service Locator | N/A | HTML has no runtime dependency registry or lookup semantics for executable services. |
| Active Object | N/A | HTML has no active object, scheduler, request queue or asynchronous method execution mechanism. |
| Monitor Object | N/A | HTML provides neither mutable shared state nor programmable locking/condition synchronization. |
| Half-Sync / Half-Async | N/A | HTML cannot establish synchronous/asynchronous execution layers or queues between them. |
| Leader / Followers | N/A | HTML has no thread/process pool, event source ownership or leader handoff semantics. |
| Client-Server | N/A | HTML is commonly transferred in client-server systems, but the markup itself implements neither client nor server process/protocol behavior. |
| Peer-to-Peer | N/A | HTML cannot discover peers, exchange application messages or participate as an executable peer. |
| Publish-Subscribe | N/A | HTML cannot own publishers, subscriptions or fan-out delivery; such behavior belongs to an executable runtime. |
| Distributed Proxy | N/A | HTML can contain a link to a remote resource, but cannot implement local proxy semantics, marshaling or remote invocation. |
| Presentation-Abstraction-Control | N/A | HTML may represent presentation output, but cannot implement PAC agents or their control/abstraction behavior. |
| Model-View-Presenter | N/A | HTML can be View markup, but cannot implement Presenter behavior or the MVP collaboration by itself. |
| Document-View | N/A | HTML can represent a document or one rendered view, but cannot coordinate an editable document model with multiple programmable views. |
| Active Record | N/A | HTML has no persistence operations, identity mapping to storage or row-oriented save/load behavior. |
| Data Mapper | N/A | HTML cannot execute mapping between domain objects and persistence representation. |
| Unit of Work | N/A | HTML has no change tracking, transaction boundary or commit/rollback execution semantics. |
| Repository | N/A | HTML cannot expose an executable collection-like persistence boundary or query/save operations. |
| Dependency Injection | N/A | Declarative nesting/attributes are document composition, not runtime provisioning of executable dependencies to consumers. |
| Lazy Initialization | N/A | Browser resource loading heuristics/attributes are platform behavior; pure HTML cannot own creation-on-first-use semantics for application objects/resources. |
| Object Pool | N/A | HTML has no programmable object lifecycle, checkout/return protocol or reusable-instance pool. |
| Null Object | N/A | Empty elements or missing markup are data/structure states, not substitutable executable collaborators implementing neutral behavior. |

## Validation boundary

There are no canonical source artifacts to compile or run because the row contains **zero Applicable cells**. Validation for this slice is therefore factual and structural:

- the target boundary is explicitly pure HTML rather than JavaScript/framework behavior;
- every one of the 39 cells has an intent-level technical justification;
- no `N/A` relies merely on absence of classes or textbook OOP syntax;
- no fake implementation path or executable evidence is claimed;
- the existing Command classification on `dev` is preserved rather than contradicted.

No code/test coverage percentage is meaningful for a zero-executable-cell applicability row; coverage is **N/A** for this slice.

## Cross-lane compatibility

This ledger does not modify `learn/**`, course metadata, navigation, validators, course workflows or production architecture. HTML Learn remains owned by the course lane and its planned status is not advanced by this classification work.

## Reconciliation rule

At horizontal closure time, each canonical pattern page must reconcile its HTML row to `N/A` with an equivalent technical reason. This ledger does not by itself change a pattern's `in-progress` status or satisfy the remaining Applicable-language cells.
