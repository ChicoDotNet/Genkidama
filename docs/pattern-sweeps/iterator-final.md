# Iterator horizontal close-out

> **Pattern:** Iterator  
> **Status:** in-progress  
> **Base audited:** `dev@57d5d439d1105ef8e380869866a697f0c4ef17bd`  
> **Canonical page:** [`wiki/Iterator.md`](../../wiki/Iterator.md)

## Purpose

This ledger owns the final horizontal reconciliation of Iterator under KB-006. Search hits and language-major runners are evidence inputs, not a language-completeness count. Every current Genkidama target must end as `Applicable` or technically justified `N/A`, and every Applicable cell must resolve to an individually addressable canonical source.

## Debt-first findings

- `wiki/Iterator.md` is currently empty on the audited `dev` base.
- Python is Applicable and originally had executable Iterator behavior only inside `src/Scripting/PythonPY/pattern_sweep.py`; that runner is not a canonical cell under KB-006.
- This branch adds `src/Scripting/PythonPY/patterns/iterator.py` and the aggregate runner now delegates its Iterator check to that canonical source instead of hiding a duplicate implementation.
- The Python target gate passed on `7fc617c799a33262caacc02cee9f8162a754dd96`; generic CI on that head is still pending at the time of this ledger update.
- The integrated portable-functional cohort contributes 13 Iterator-Applicable canonical sources and the integrated medium-high cohort contributes 14 more; both ledgers explicitly guarantee individually addressable canonical artifacts for their cells.
- The integrated high-overhead cohort classifies Haskell, Dart, Crystal, Zig, Julia, Go and Objective-C as Iterator-Applicable, but its historical ledger points to seven 39-pattern sweep runners. Under the later approved KB-006 canonical-cell rule those runners are behavioral evidence/orchestration, not final canonical artifacts. These seven Iterator cells therefore remain canonical-source gaps until extracted and the runners no longer hide duplicate implementations.
- Pure HTML and pure CSS are both Iterator `N/A`: document/style evaluation does not expose author-controlled traversal state or a programmable `next/current` iteration protocol. These are intent-level exclusions, not OOP-based shortcuts.
- Existing `iterator` search results use several naming/layout conventions, so raw search-result counts must not be reported as the final `implemented/applicable` denominator.

## Audited applicability and canonical-source state

| Slice / lenguaje | Aplicabilidad | Fuente canónica | Estado | Evidencia |
|---|---|---|---|---|
| Portable-functional cohort (13 targets) | 13 Applicable | `patterns/*` per target | 13 canonical on `dev` | Cohort ledger requires individually addressable sources and native/runtime verification |
| Medium-high cohort (14 targets) | 14 Applicable | `patterns/*` / ecosystem-native equivalents | 14 canonical on `dev` | Cohort ledger requires individually addressable sources and behavioral verification |
| Python | Applicable | [`iterator.py`](../../src/Scripting/PythonPY/patterns/iterator.py) | canonical; runner deduplicated; target gate green | Native `__iter__` + generator; repeatable traversal contract |
| Haskell | Applicable | — | canonical gap | Existing `PatternSweep.hs` behavior is runner-only evidence under current KB-006 |
| Dart | Applicable | — | canonical gap | Existing `pattern_sweep.dart` behavior is runner-only evidence under current KB-006 |
| Crystal | Applicable | — | canonical gap | Existing `pattern_sweep.cr` behavior is runner-only evidence under current KB-006 |
| Zig | Applicable | — | canonical gap | Existing `pattern_sweep.zig` behavior is runner-only evidence under current KB-006 |
| Julia | Applicable | — | canonical gap | Existing `pattern_sweep.jl` behavior is runner-only evidence under current KB-006 |
| Go | Applicable | — | canonical gap | Existing `pattern_sweep.go` behavior is runner-only evidence under current KB-006 |
| Objective-C | Applicable | — | canonical gap | Existing `pattern_sweep.m` behavior is runner-only evidence under current KB-006 |
| HTML | N/A | — | classified | No programmable author-controlled traversal cursor/protocol in pure markup |
| CSS | N/A | — | classified | Selector matching is browser style evaluation, not an author-controlled iteration protocol |

Current audited subtotal: **35 Applicable targets, 28 with canonical sources, 7 canonical gaps; 2 N/A**. This is an audited subtotal, not the final 51-target denominator. The remaining targets must still be reconciled before Iterator can claim final `implemented/applicable`.

## Exit boundary

Iterator remains `in-progress` until all 51 current targets have been reconciled, every Applicable target has a linked canonical source with the strongest reasonable validation, every N/A has an intent-level justification, aggregate runners contain no hidden duplicate canonical implementations, and `wiki/Iterator.md` satisfies the full KB-006 page DoD.

No code/test coverage percentage is claimed for the heterogeneous polyglot matrix unless an ecosystem exposes a meaningful measurement. The repository 44% floor remains controlling where coverage is measurable.
