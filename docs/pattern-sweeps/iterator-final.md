# Iterator horizontal close-out

> **Pattern:** Iterator  
> **Status:** in-progress  
> **Base audited:** `dev@57d5d439d1105ef8e380869866a697f0c4ef17bd`  
> **Canonical page:** [`wiki/Iterator.md`](../../wiki/Iterator.md)

## Purpose

This ledger owns the final horizontal reconciliation of Iterator under KB-006. Search hits and language-major runners are evidence inputs, not a language-completeness count. Every current Genkidama target must end as `Applicable` or technically justified `N/A`, and every Applicable cell must resolve to an individually addressable canonical source.

## Final applicability denominator

The current target universe is closed at **51 targets = 49 Applicable + 2 N/A**. HTML and CSS are the only `N/A` cells: pure markup/style evaluation does not expose author-controlled traversal state or a programmable `next/current` protocol. These are intent-level exclusions, not OOP-based shortcuts.

All **49 Applicable** targets now have an individually addressable canonical Iterator source. Existing language-major/cohort evidence accounts for 41 canonical cells; this branch materializes the eight previously missing tail cells: SQL, Perl, VBA, GDScript, Assembly, Delphi, MicroPython and Rockstar.

## Debt-first findings

- `wiki/Iterator.md` is still incomplete on this branch and remains required before promotion.
- Python originally had Iterator only inside `src/Scripting/PythonPY/pattern_sweep.py`; this branch added `src/Scripting/PythonPY/patterns/iterator.py` and made the aggregate runner delegate to it. The Python gate is green.
- Portable-functional contributes 13 verified canonical Iterator cells; medium-high contributes 14 verified canonical cells.
- Haskell, Dart, Crystal, Zig, Julia, Go and Objective-C now have individually addressable canonical files and a green dedicated high-overhead gate. Their seven historical sweep runners still contain duplicate Iterator implementations; runner deduplication remains bounded debt because KB-006 permits orchestration, not hidden duplicate canonical implementations.
- Existing language-major canonical sources cover PHP, JavaScript, Ruby, Lua, Bash and MATLAB.
- The eight tail sources added here are validated by `.github/workflows/pattern-iterator-tail.yml`: SQL/Perl/Assembly by native execution, GDScript by Godot, MicroPython by its Unix runtime, Rockstar by the official runtime, and VBA/Delphi by source contracts where hosted Linux cannot reasonably execute the proprietary toolchains.
- The Delphi source is self-contained (`SysUtils` imported for `Exception`) rather than merely matching the source contract.

## Applicability and canonical-source state

| Slice / language | Applicability | Canonical state | Validation state |
|---|---:|---:|---|
| Portable-functional cohort | 13 Applicable | 13/13 | verified on integrated cohort |
| Medium-high cohort | 14 Applicable | 14/14 | verified on integrated cohort |
| Python | 1 Applicable | 1/1 | Python target gate green |
| High-overhead: Haskell, Dart, Crystal, Zig, Julia, Go, Objective-C | 7 Applicable | 7/7 | dedicated canonical gate green |
| Existing language-major: PHP, JavaScript, Ruby, Lua, Bash, MATLAB | 6 Applicable | 6/6 | canonical sources already integrated/validated by their language-major lanes |
| Tail: SQL, Perl, VBA, GDScript, Assembly, Delphi, MicroPython, Rockstar | 8 Applicable | 8/8 | dedicated tail gate recertifying current head |
| HTML | N/A | — | pure markup has no author-programmable traversal cursor/protocol |
| CSS | N/A | — | selector matching is not author-controlled iteration state/current/next |

Current materialization: **49/49 Applicable canonical sources**. Verification is complete for the previously audited 41 cells; the eight tail cells are materialized and their dedicated gate is running on the current head. Do not promote this ledger to `validated` until that gate and the final page/CI head are green.

## Tail canonical paths

- SQL: [`src/Data/SQL/iterator.sql`](../../src/Data/SQL/iterator.sql)
- Perl: [`src/Scripting/Perl/iterator.pl`](../../src/Scripting/Perl/iterator.pl)
- VBA: [`src/Shell/VBA/IteratorExample.bas`](../../src/Shell/VBA/IteratorExample.bas)
- GDScript: [`src/Niche/GDScript/iterator.gd`](../../src/Niche/GDScript/iterator.gd)
- Assembly: [`src/LowLevel/Assembly/iterator.asm`](../../src/LowLevel/Assembly/iterator.asm)
- Delphi: [`src/Enterprise/Delphi/IteratorExample.pas`](../../src/Enterprise/Delphi/IteratorExample.pas)
- MicroPython: [`src/Other/MicroPython/iterator.py`](../../src/Other/MicroPython/iterator.py)
- Rockstar: [`src/Other/Rockstar/iterator.rock`](../../src/Other/Rockstar/iterator.rock)

## Exit boundary

Iterator remains `in-progress` until the current head certifies all applicable gates, the seven high-overhead runners are deduplicated into orchestration-only roles, `wiki/Iterator.md` satisfies the full KB-006 page DoD with links to all 49 Applicable sources and the two defensible N/A cells, and the final PR head remains mergeable against the then-current `dev` after the 20% Learn compatibility check.

No aggregate code/test coverage percentage is claimed for this heterogeneous polyglot matrix. The repository 44% floor remains controlling where coverage is meaningfully measurable; otherwise the strongest lightweight ecosystem validation is used without inventing numbers.
