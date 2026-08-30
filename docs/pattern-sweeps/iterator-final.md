# Iterator horizontal close-out

> **Pattern:** Iterator  
> **Status:** validated  
> **Base audited:** `dev@378de71f119beca2e69c2d1a22024cb0680eceee`  
> **Canonical page:** [`wiki/Iterator.md`](../../wiki/Iterator.md)

## Purpose

This ledger owns the final horizontal reconciliation of Iterator under KB-006. Search hits and language-major runners are evidence inputs, not a language-completeness count. Every current Genkidama target must end as `Applicable` or technically justified `N/A`, and every Applicable cell must resolve to an individually addressable canonical source.

## Final applicability denominator

The current target universe is closed at **51 targets = 49 Applicable + 2 N/A**. HTML and CSS are the only `N/A` cells: pure markup/style evaluation does not expose author-controlled traversal state or a programmable `next/current` protocol. These are intent-level exclusions, not OOP-based shortcuts.

All **49 Applicable** targets have an individually addressable canonical Iterator source. Existing language-major/cohort evidence accounts for 41 canonical cells; this branch materializes the eight previously missing tail cells: SQL, Perl, VBA, GDScript, Assembly, Delphi, MicroPython and Rockstar.

## Debt-first findings

- `wiki/Iterator.md` is fully authored to the KB-006 content structure with the complete 51-target table and is now `validated` after runner deduplication and certification.
- Python originally had Iterator only inside `src/Scripting/PythonPY/pattern_sweep.py`; this branch added `src/Scripting/PythonPY/patterns/iterator.py` and made the aggregate runner delegate to it. The Python gate is green.
- Portable-functional contributes 13 verified canonical Iterator cells; medium-high contributes 14 verified canonical cells.
- Haskell, Dart, Crystal, Zig, Julia, Go and Objective-C have individually addressable canonical files and dedicated high-overhead validation.
- **Haskell runner debt is paid:** `src/Functional/Haskell/PatternSweep.hs` no longer contains a duplicate Iterator implementation; it delegates execution to `src/Functional/Haskell/Iterator.hs`. Head `4dca5b4fcde8aa59eea0814c1ef88290ab4db8c7` passed all nine then-applicable PR workflows, including the high-overhead cohort and dedicated Iterator gate.
- **Julia runner debt is paid:** `src/DataScience/Julia/pattern_sweep.jl` includes the canonical `src/DataScience/Julia/iterator.jl` rather than defining a second Iterator. The canonical file remains directly executable while staying silent when included. `Pattern High-Overhead Cohort Sweep #27` passed on head `7d1dedf4e156e2145647f5e32666e93cd5420037` against `dev@bea35ba3…`.
- **Dart runner debt is paid:** `src/Web/Dart/pattern_sweep.dart` imports the canonical `src/Web/Dart/iterator.dart`; its high-overhead sweep is certified with format, analyze and 39/39 execution.
- **Crystal runner debt is paid:** `src/Niche/Crystal/pattern_sweep.cr` requires the canonical `src/Niche/Crystal/iterator.cr` and delegates to `run_iterator_example`. The repaired dedicated canonical harness passed on head `9749bead8a037271d188fe0898fcdd426c8d266a`, and the aggregate Crystal 39/39 job is green on that head.
- **Zig runner debt is paid:** `src/Systems/Zig/pattern_sweep.zig` imports `src/Systems/Zig/iterator.zig` and delegates to its exported `runIteratorExample`; both the dedicated Zig Iterator job and the high-overhead aggregate Zig 39/39 job passed on head `9749bead8a037271d188fe0898fcdd426c8d266a`.
- **Go runner debt is paid:** `src/Systems/Go/pattern_sweep.go` executes the canonical `src/Systems/Go/iterator.go` rather than carrying its own `intIterator`; the canonical and aggregate high-overhead gates are green.
- **Objective-C runner debt is paid:** `src/Systems/Objective-C/pattern_sweep.m` imports the canonical `src/Systems/Objective-C/iterator.m` with its standalone `main` disabled and delegates to the canonical `iteratorExamplePasses` validation. Head `cb06f0eb4023e4e3fd8e579d71a374331edf0dab` passed all eleven applicable PR workflows, including `Pattern Iterator High-Overhead Canonical #32` and `Pattern High-Overhead Cohort Sweep #38`; the latter preserves the strict Objective-C compile/runtime aggregate contract.
- No historical high-overhead runner retains a second hidden Iterator implementation.
- Existing language-major canonical sources cover PHP, JavaScript, Ruby, Lua, Bash and MATLAB.
- The eight tail sources added here are validated by `.github/workflows/pattern-iterator-tail.yml`: SQL/Perl/Assembly by native execution, GDScript by Godot, MicroPython by its Unix runtime, Rockstar by the official runtime, and VBA/Delphi by source contracts where hosted Linux cannot reasonably execute the proprietary toolchains.
- The Delphi source is self-contained (`SysUtils` imported for `Exception`) rather than merely matching the source contract.

## Applicability and canonical-source state

| Slice / language | Applicability | Canonical state | Validation state |
|---|---:|---:|---|
| Portable-functional cohort | 13 Applicable | 13/13 | verified on integrated cohort |
| Medium-high cohort | 14 Applicable | 14/14 | verified on integrated cohort |
| Python | 1 Applicable | 1/1 | Python target gate green |
| High-overhead: Haskell | 1 Applicable | 1/1 | canonical source verified; historical runner delegates to canonical source |
| High-overhead: Julia | 1 Applicable | 1/1 | canonical source verified; historical runner delegation certified by high-overhead cohort #27 on `7d1dedf4…` |
| High-overhead: Dart | 1 Applicable | 1/1 | canonical source verified; historical runner delegation certified |
| High-overhead: Crystal | 1 Applicable | 1/1 | canonical + aggregate delegation certified on `9749bead…` |
| High-overhead: Zig | 1 Applicable | 1/1 | canonical + aggregate delegation certified on `9749bead…` |
| High-overhead: Go | 1 Applicable | 1/1 | canonical + aggregate delegation certified |
| High-overhead: Objective-C | 1 Applicable | 1/1 | canonical + aggregate delegation certified on `cb06f0eb…` |
| Existing language-major: PHP, JavaScript, Ruby, Lua, Bash, MATLAB | 6 Applicable | 6/6 | canonical sources already integrated/validated by their language-major lanes |
| Tail: SQL, Perl, VBA, GDScript, Assembly, Delphi, MicroPython, Rockstar | 8 Applicable | 8/8 | dedicated tail gate green on final code head `cb06f0eb…` |
| HTML | N/A | — | pure markup has no author-programmable traversal cursor/protocol |
| CSS | N/A | — | selector matching is not author-controlled iteration state/current/next |

Current materialization: **49/49 Applicable canonical sources**. The full denominator is reconciled and the bounded runner-duplication debt is paid.

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

Iterator is `validated`: all 49 Applicable targets have canonical examples, both N/A cells are technically justified, all historical high-overhead runners delegate to canonical Iterator sources instead of hiding second implementations, and the final code head `cb06f0eb4023e4e3fd8e579d71a374331edf0dab` passed all eleven applicable PR workflows. Promotion still requires the resulting documentation head to recertify green and remain mergeable against the then-current `dev` after the 20% Learn compatibility check.

No aggregate code/test coverage percentage is claimed for this heterogeneous polyglot matrix. The repository 44% floor remains controlling where coverage is meaningfully measurable; otherwise the strongest lightweight ecosystem validation is used without inventing numbers.
