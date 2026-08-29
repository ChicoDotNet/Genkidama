# Interpreter — final KB-006 reconciliation

This ledger records factual progress for the horizontal Interpreter close-out. The canonical pattern page remains `wiki/Interpreter.md`; this file must not be used to claim pattern completion before the full KB-006 language table is reconciled.

## Baseline

- Base: `dev@57d5d439d1105ef8e380869866a697f0c4ef17bd`.
- KB-006 is approved on `dev`.
- `wiki/Interpreter.md` is still the historical/incomplete pattern page and is not yet a KB-006 completion claim.
- The current target universe is 51 targets; **Fortran is part of that denominator**. Filename-only search results are not used as an implementation count because the repository has multiple naming conventions (`interpreter`, `Interpreter`, `interpreter_pattern`, and language-specific layouts).

## Debt-first increments

The audit has identified and materialized nine unambiguous canonical-source gaps so far:

1. **SQL** — `src/Data/SQL/interpreter.sql` models `Expr := Number ('+' Number)*` as relational tokens and interprets it with a recursive SQLite CTE. Behavioral validation: `2 + 3 + 4` => `value=9`.
2. **Go** — `src/Systems/Go/interpreter.go` models grammar nodes as an `Expr` interface implemented by `Number` and recursive `Add` structs.
3. **Haskell** — `src/Functional/Haskell/Interpreter.hs` models the grammar as an algebraic data type and interprets it through structural recursion.
4. **Julia** — `src/DataScience/Julia/interpreter.jl` models grammar nodes as an abstract type with `Number` and recursive `Add` variants using multiple dispatch.
5. **Dart** — `src/Web/Dart/interpreter.dart` uses a sealed expression hierarchy and recursive interpretation.
6. **Crystal** — `src/Niche/Crystal/interpreter.cr` uses an abstract expression contract and recursive concrete grammar nodes.
7. **Zig** — `src/Systems/Zig/interpreter.zig` represents the tiny grammar as tagged tokens and interprets `Number ('+' Number)*` without forcing an object model.
8. **Objective-C** — `src/Systems/Objective-C/interpreter.m` uses an expression protocol plus recursive number/addition objects.
9. **Nim** — `src/Niche/Nim/interpreter.nim` models a recursive discriminated expression tree and interprets it structurally.

The latest audit intentionally replaces cohort-only addressability with individual canonical artifacts where a per-pattern source was actually absent. Dart, Crystal, Zig and Objective-C were already `Applicable` and behaviorally materialized inside the high-overhead 39-cell sweep; Nim likewise existed only inside its language-major sweep source for Interpreter. Under KB-006 those aggregate runners are validation/orchestration evidence, not substitutes for a per-pattern canonical source.

The audit also corrected false gaps caused by naming/layout differences. Canonical Interpreter sources already exist for Ada, Pascal, Clojure, COBOL, Solidity, MATLAB, Lua, Bash, **Scala** (`src/Functional/Scala/patterns/Interpreter.scala`) and **Fortran** (`src/Systems/Fortran/patterns/interpreter.f90`), among others. Duplicate close-out files for Scala and Fortran were removed rather than retained. These discoveries are inventory facts, not a claim that the complete 51-target matrix is already verified.

## Validation

`Pattern Interpreter Final` is the horizontal certification boundary for the nine canonical artifacts added during this close-out. It performs compile/analyze/runtime checks using current stable or distro-stable toolchains and requires the shared teaching contract `2 + 3 + 4 = 9`. A green run certifies these artifacts individually; the historical language-major cohort gates remain evidence for their broader rows but are not substituted for this horizontal gate.

## Remaining reconciliation

Before Interpreter can be marked `validated`:

1. finish the complete 51-target Applicable/N/A inventory under KB-006;
2. inspect every candidate source for canonical per-cell addressability and intent fidelity;
3. implement and validate every remaining Applicable gap;
4. reconcile `wiki/Interpreter.md` with the full table, trade-offs, use/no-use guidance, relationships/confusions, Mermaid, verification, and factual `En Genkidama` usage;
5. certify the reviewed head and reconcile any concurrent movement of `dev`.

No aggregate code/test coverage percentage is claimed for the polyglot set. Where percentage coverage is meaningful the repository policy applies; otherwise compile/analyze/runtime evidence is preferred and no number is invented.
