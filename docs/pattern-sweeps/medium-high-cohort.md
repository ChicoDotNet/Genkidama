# Medium-high Design Pattern cohort sweep

> **State:** materialized; verification is owned by `Pattern Medium-High Cohort Sweep` on the reviewed head.  
> **Scope:** 14 target languages × 39 patterns that remained incomplete after Chain of Responsibility = **546 matrix cells**.  
> **Applicability:** 546 Applicable, 0 N/A in this cohort.  
> **Promotion:** this ledger certifies only these target-language cells; it does not mark any canonical pattern `validated`.

## Targets

| Target | Cells | Source | Validation owner |
|---|---:|---|---|
| Scala | 39 | [`PatternSweep.scala`](../../src/Functional/Scala/PatternSweep.scala) | JVM job |
| Clojure | 39 | [`pattern_sweep.clj`](../../src/Functional/Clojure/pattern_sweep.clj) | JVM job |
| Kotlin | 39 | [`PatternSweep.kt`](../../src/Enterprise/Kotlin/PatternSweep.kt) | JVM job |
| Swift | 39 | [`pattern_sweep.swift`](../../src/Systems/Swift/pattern_sweep.swift) | Swift stable job |
| C# | 39 | [`PatternSweep.cs`](../../src/Enterprise/C%23/PatternSweep.cs) | .NET 10 LTS job |
| Visual Basic .NET | 39 | [`PatternSweep.vb`](../../src/Enterprise/VB.NET/PatternSweep.vb) | .NET 10 LTS job |
| F# | 39 | [`pattern_sweep.fsx`](../../src/Functional/F%23/pattern_sweep.fsx) | .NET 10 LTS job |
| Solidity | 39 | [`PatternSweep.sol`](../../src/Niche/Solidity/PatternSweep.sol) | Node 24 / latest stable solc job |
| Ada | 39 | [`pattern_sweep.adb`](../../src/Systems/Ada/pattern_sweep.adb) | GNU compiled job |
| Pascal | 39 | [`pattern_sweep.pas`](../../src/Systems/Pascal/pattern_sweep.pas) | GNU compiled job |
| COBOL | 39 | [`pattern_sweep.cbl`](../../src/Historical/Cobol/pattern_sweep.cbl) | GNU compiled job |
| Fortran | 39 | [`pattern_sweep.f90`](../../src/Systems/Fortran/pattern_sweep.f90) | GNU compiled job |
| Nim | 39 | [`pattern_sweep.nim`](../../src/Niche/Nim/pattern_sweep.nim) | Nim stable job |
| TypeScript | 39 | [`pattern-sweep.ts`](../../src/Web/TypeScriptTS/pattern-sweep.ts) | Node 24 job |

## Pattern cells

Each target source contains an isolated behavioral check for the same 39 catalog entries: Command, Interpreter, Iterator, Mediator, Memento, Observer, State, Strategy, Template Method, Visitor, MVC, MVVM, Microkernel, Microservices, Enterprise Adapter, Enterprise Bridge, Enterprise Facade, Broker, Message Bus, Service Locator, Active Object, Monitor Object, Half-Sync / Half-Async, Leader / Followers, Client-Server, Peer-to-Peer, Publish-Subscribe, Distributed Proxy, Presentation-Abstraction-Control, Model-View-Presenter, Document-View, Active Record, Data Mapper, Unit of Work, Repository, Dependency Injection, Lazy Initialization, Object Pool and Null Object.

## CI amortization

The cohort deliberately pays **six setup contexts rather than fourteen**:

| Setup context | Targets | Cells |
|---|---|---:|
| JVM | Scala, Clojure, Kotlin | 117 |
| .NET 10 LTS | C#, VB.NET, F# | 117 |
| Node 24 LTS | TypeScript, Solidity | 78 |
| GNU compiled | Ada, Pascal, COBOL, Fortran | 156 |
| Swift stable | Swift | 39 |
| Nim stable | Nim | 39 |

The gate records setup, validation and total seconds per context. A green reviewed head means all **546 cells** passed their target's strongest practical lightweight gate.

## Toolchain freshness

This sweep follows the owner rule that pattern CI uses the **most recent stable or LTS toolchain reasonably available**.

- Prefer official `stable`, `latest` or LTS channels over stale hard-coded compiler versions.
- CI actions themselves must not depend on a GitHub Actions runtime that GitHub has deprecated when a current alternative exists.
- When the ecosystem's current action remains on a deprecated Node runtime, install from an official stable distribution/channel instead.
- Do not enable dependency caching when the repository slice does not contain the dependency manifest required by that cache.
- For distro-packaged GNU-family compilers, use the current `ubuntu-latest` stable repositories and record the resolved version; do not pin an older package merely for historical reproducibility.

These rules affect toolchain freshness, not the pattern Definition of Done. They must never be used to weaken compile/analyze/runtime evidence.

## Evidence boundary

Before the cohort workflow runs, these files are **materialized but not yet called verified**. After a green `Pattern Medium-High Cohort Sweep` on the reviewed head, that run is the verification evidence for this ledger.

Canonical pattern pages remain `in-progress` until every Applicable target in the complete 51-target universe is reconciled and verified under KB-006.
