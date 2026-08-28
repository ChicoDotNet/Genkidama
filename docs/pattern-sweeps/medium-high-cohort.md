# Medium-high Design Pattern cohort sweep

> **State:** canonicalized and behaviorally verified on the reviewed branch; final promotion still requires the current head to be green and reconciled with `dev`.  
> **Scope:** 14 target languages × 39 patterns that remained incomplete after Chain of Responsibility = **546 matrix cells**.  
> **Applicability:** **546 Applicable, 0 N/A** in this cohort.  
> **Promotion boundary:** this ledger certifies only these target-language cells; it does not mark any canonical pattern page `validated`.

## Canonical-source rule

Every cell in this ledger has an addressable source artifact whose primary purpose is that one pattern in that one target language. Sweep files are runners/orchestrators only: they may import, include, load, compile or execute the canonical artifacts so CI can amortize toolchain setup, but they do not substitute for the canonical examples and must not hide duplicate pattern implementations.

## Targets

| Target | Cells | Canonical sources | Runner / orchestrator | Validation owner |
|---|---:|---|---|---|
| Scala | 39 | [`patterns/`](../../src/Functional/Scala/patterns/) | [`PatternSweep.scala`](../../src/Functional/Scala/PatternSweep.scala) | JVM cohort |
| Clojure | 39 | [`patterns/`](../../src/Functional/Clojure/patterns/) | [`pattern_sweep.clj`](../../src/Functional/Clojure/pattern_sweep.clj) | JVM cohort |
| Kotlin | 39 | [`patterns/`](../../src/Enterprise/Kotlin/patterns/) | [`PatternSweep.kt`](../../src/Enterprise/Kotlin/PatternSweep.kt) | JVM cohort |
| Swift | 39 | [`patterns/`](../../src/Systems/Swift/patterns/) | [`pattern_sweep.swift`](../../src/Systems/Swift/pattern_sweep.swift) | Swift canonical + cohort |
| C# | 39 | [`patterns/`](../../src/Enterprise/C%23/patterns/) | [`PatternSweep.cs`](../../src/Enterprise/C%23/PatternSweep.cs) | .NET 10 LTS cohort |
| Visual Basic .NET | 39 | [`patterns/`](../../src/Enterprise/VB.NET/patterns/) | [`PatternSweep.vb`](../../src/Enterprise/VB.NET/PatternSweep.vb) | .NET 10 LTS cohort |
| F# | 39 | [`patterns/`](../../src/Functional/F%23/patterns/) | [`pattern_sweep.fsx`](../../src/Functional/F%23/pattern_sweep.fsx) | .NET 10 LTS cohort |
| Solidity | 39 | [`patterns/`](../../src/Niche/Solidity/patterns/) | [`PatternSweep.sol`](../../src/Niche/Solidity/PatternSweep.sol) | Solidity canonical + Node cohort |
| Ada | 39 | [`patterns/`](../../src/Systems/Ada/patterns/) | [`pattern_sweep.adb`](../../src/Systems/Ada/pattern_sweep.adb) | Ada canonical + GNU cohort |
| Pascal | 39 | [`*_pattern.pas`](../../src/Systems/Pascal/) | [`pattern_sweep.pas`](../../src/Systems/Pascal/pattern_sweep.pas) | Pascal canonical + GNU cohort |
| COBOL | 39 | [`patterns/*.cpy`](../../src/Historical/Cobol/patterns/) | [`pattern_sweep.cbl`](../../src/Historical/Cobol/pattern_sweep.cbl) | COBOL canonical + GNU cohort |
| Fortran | 39 | [`patterns/`](../../src/Systems/Fortran/patterns/) | [`pattern_sweep.f90`](../../src/Systems/Fortran/pattern_sweep.f90) | Fortran canonical + GNU cohort |
| Nim | 39 | [`patterns/`](../../src/Niche/Nim/patterns/) | [`pattern_sweep.nim`](../../src/Niche/Nim/pattern_sweep.nim) | Nim canonical + cohort |
| TypeScript | 39 | [`patterns/`](../../src/Web/TypeScriptTS/patterns/) | [`pattern-sweep.ts`](../../src/Web/TypeScriptTS/pattern-sweep.ts) | Node 24 cohort |

## Pattern cells

Each target covers the same 39 catalog entries: Command, Interpreter, Iterator, Mediator, Memento, Observer, State, Strategy, Template Method, Visitor, MVC, MVVM, Microkernel, Microservices, Enterprise Adapter, Enterprise Bridge, Enterprise Facade, Broker, Message Bus, Service Locator, Active Object, Monitor Object, Half-Sync / Half-Async, Leader / Followers, Client-Server, Peer-to-Peer, Publish-Subscribe, Distributed Proxy, Presentation-Abstraction-Control, Model-View-Presenter, Document-View, Active Record, Data Mapper, Unit of Work, Repository, Dependency Injection, Lazy Initialization, Object Pool and Null Object.

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

Per-language canonical gates were used during structural extraction where they bought useful diagnostic isolation. The final cohort gate installs each expensive context once and verifies its complete aggregate. This preserves addressable examples without returning to one-runtime-per-pattern CI fragmentation.

## Toolchain freshness

This sweep follows KB-006: pattern CI uses the **most recent stable or LTS toolchain reasonably available** at validation time.

- Prefer official `stable`, `latest` or current LTS channels over stale compiler pins.
- Do not rely on a GitHub Action whose embedded runtime is deprecated when a current alternative is reasonably available.
- When an ecosystem action remains stale, install from the ecosystem's official stable distribution/channel and verify integrity where practical.
- Do not enable dependency caching without the manifest/cache inputs that make that cache valid.
- Distro-managed GNU-family compilers come from the current `ubuntu-latest` LTS repositories and their resolved versions are observable in CI.

Freshness never weakens pattern validation; it only prevents a green catalog from silently depending on obsolete tooling.

## Evidence boundary

A cell is called verified only after its required native gate actually passes. Materialization alone is not verification. The reviewed cohort head must also have the generic repository CI green.

Canonical pattern pages remain `in-progress` until every Applicable target in the full 51-target universe is reconciled and verified under KB-006. This sweep therefore does **not** change the global count of completed patterns by itself.
