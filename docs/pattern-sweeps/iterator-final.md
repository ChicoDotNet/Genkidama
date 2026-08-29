# Iterator horizontal close-out

> **Pattern:** Iterator  
> **Status:** in-progress  
> **Base audited:** `dev@57d5d439d1105ef8e380869866a697f0c4ef17bd`  
> **Canonical page:** [`wiki/Iterator.md`](../../wiki/Iterator.md)

## Purpose

This ledger owns the final horizontal reconciliation of Iterator under KB-006. Search hits and language-major runners are evidence inputs, not a language-completeness count. Every current Genkidama target must end as `Applicable` or technically justified `N/A`, and every Applicable cell must resolve to an individually addressable canonical source.

## Debt-first findings

- `wiki/Iterator.md` is currently empty on the audited `dev` base.
- Python is Applicable and originally had executable Iterator behavior only inside `src/Scripting/PythonPY/pattern_sweep.py`; this branch added `src/Scripting/PythonPY/patterns/iterator.py` and changed the aggregate runner to delegate to the canonical source. The Python target gate passed on `7fc617c799a33262caacc02cee9f8162a754dd96`.
- The integrated portable-functional cohort contributes 13 Iterator-Applicable canonical sources and the integrated medium-high cohort contributes 14 more; both ledgers guarantee individually addressable canonical artifacts for their cells.
- The integrated high-overhead cohort classifies Haskell, Dart, Crystal, Zig, Julia, Go and Objective-C as Iterator-Applicable, but its historical ledger points to seven 39-pattern sweep runners. This branch now materializes individually addressable Iterator sources for all seven: `src/Functional/Haskell/Iterator.hs`, `src/Web/Dart/iterator.dart`, `src/Niche/Crystal/iterator.cr`, `src/Systems/Zig/iterator.zig`, `src/DataScience/Julia/iterator.jl`, `src/Systems/Go/iterator.go` and `src/Systems/Objective-C/iterator.m`.
- `.github/workflows/pattern-iterator-high-overhead.yml` validates those seven canonical sources with current stable/LTS toolchains using format/analyze/compile/runtime evidence as appropriate. They remain **materialized but unverified** until that workflow is green on the reviewed head.
- The seven historical high-overhead runners still contain duplicate Iterator implementations. Under KB-006 they may remain orchestration only, so runner deduplication is bounded debt that must be paid before Iterator promotion; the new canonical files do not erase that debt.
- Pure HTML and pure CSS are both Iterator `N/A`: document/style evaluation does not expose author-controlled traversal state or a programmable `next/current` iteration protocol. These are intent-level exclusions, not OOP-based shortcuts.
- Existing `iterator` search results use several naming/layout conventions, so raw search-result counts must not be reported as the final `implemented/applicable` denominator.

## Audited applicability and canonical-source state

| Slice / lenguaje | Aplicabilidad | Fuente canónica | Estado | Evidencia |
|---|---|---|---|---|
| Portable-functional cohort (13 targets) | 13 Applicable | `patterns/*` per target | 13 canonical on `dev` | Cohort ledger requires individually addressable sources and native/runtime verification |
| Medium-high cohort (14 targets) | 14 Applicable | `patterns/*` / ecosystem-native equivalents | 14 canonical on `dev` | Cohort ledger requires individually addressable sources and behavioral verification |
| Python | Applicable | [`iterator.py`](../../src/Scripting/PythonPY/patterns/iterator.py) | canonical; runner deduplicated; target gate green | Native `__iter__` + generator; repeatable traversal contract |
| Haskell | Applicable | [`Iterator.hs`](../../src/Functional/Haskell/Iterator.hs) | canonical materialized; validation pending; runner duplicate remains | Explicit immutable iterator state returning `(Maybe value, nextIterator)` |
| Dart | Applicable | [`iterator.dart`](../../src/Web/Dart/iterator.dart) | canonical materialized; validation pending; runner duplicate remains | Generic cursor with `hasNext` / `next` traversal |
| Crystal | Applicable | [`iterator.cr`](../../src/Niche/Crystal/iterator.cr) | canonical materialized; validation pending; runner duplicate remains | Cursor object returning successive optional values |
| Zig | Applicable | [`iterator.zig`](../../src/Systems/Zig/iterator.zig) | canonical materialized; validation pending; runner duplicate remains | Explicit slice + index state with optional `next` |
| Julia | Applicable | [`iterator.jl`](../../src/DataScience/Julia/iterator.jl) | canonical materialized; validation pending; runner duplicate remains | Idiomatic `Base.iterate` protocol with explicit state |
| Go | Applicable | [`iterator.go`](../../src/Systems/Go/iterator.go) | canonical materialized; validation pending; runner duplicate remains | Generic iterator returning `(value, ok)` |
| Objective-C | Applicable | [`iterator.m`](../../src/Systems/Objective-C/iterator.m) | canonical materialized; validation pending; runner duplicate remains | Cocoa-style iterator object over an encapsulated collection |
| HTML | N/A | — | classified | No programmable author-controlled traversal cursor/protocol in pure markup |
| CSS | N/A | — | classified | Selector matching is browser style evaluation, not an author-controlled iteration protocol |

Current audited subtotal: **35 Applicable targets, 35 with canonical sources materialized; 2 N/A**. Of those 35 Applicable targets, seven newly extracted high-overhead cells await their dedicated gate and their historical runners still require deduplication. This is an audited subtotal, not the final 51-target denominator. The remaining targets must still be reconciled before Iterator can claim final `implemented/applicable`.

## Exit boundary

Iterator remains `in-progress` until all 51 current targets have been reconciled, every Applicable target has a linked canonical source with the strongest reasonable validation, every N/A has an intent-level justification, aggregate runners contain no hidden duplicate canonical implementations, and `wiki/Iterator.md` satisfies the full KB-006 page DoD.

No code/test coverage percentage is claimed for the heterogeneous polyglot matrix unless an ecosystem exposes a meaningful measurement. The repository 44% floor remains controlling where coverage is measurable.
