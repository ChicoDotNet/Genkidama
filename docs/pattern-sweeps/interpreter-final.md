# Interpreter — final KB-006 reconciliation

This ledger records factual progress for the horizontal Interpreter close-out. The canonical pattern page remains `wiki/Interpreter.md`; this file must not be used to claim pattern completion before the full KB-006 page is reconciled.

## Baseline

- Base: `dev@57d5d439d1105ef8e380869866a697f0c4ef17bd`.
- KB-006 is approved on `dev`.
- Current target universe: **51 targets**.
- Final applicability inventory: **49 Applicable, 2 N/A**.
- `HTML` and `CSS` are the only N/A targets. HTML can describe syntax-shaped markup but cannot itself execute an evaluator for another grammar; CSS is a browser-evaluated styling rule language and does not provide an author-programmable evaluation mechanism for such a grammar. These exclusions are based on Interpreter intent, not on lack of classes/OOP.
- `wiki/Interpreter.md` is still empty on this branch and is not yet a KB-006 completion claim.

## Canonical-source audit

The complete 49-Applicable inventory now has an individually addressable canonical source for Interpreter on this branch.

Existing canonical coverage was established through integrated language-major ledgers and individual target extractions. During final horizontal reconciliation, every claim that mattered was checked against the actual source layout rather than treating a sweep runner as a canonical cell.

The horizontal audit materialized **17 canonical Interpreter gaps** in this PR:

1. **SQL** — `src/Data/SQL/interpreter.sql`: relational tokens + recursive SQLite CTE.
2. **Go** — `src/Systems/Go/interpreter.go`: `Expr` interface with recursive number/add nodes.
3. **Haskell** — `src/Functional/Haskell/Interpreter.hs`: ADT + structural recursion.
4. **Julia** — `src/DataScience/Julia/interpreter.jl`: abstract expression type + multiple dispatch.
5. **Dart** — `src/Web/Dart/interpreter.dart`: sealed expression hierarchy.
6. **Crystal** — `src/Niche/Crystal/interpreter.cr`: abstract expression contract + recursive nodes.
7. **Zig** — `src/Systems/Zig/interpreter.zig`: tagged tokens + evaluator.
8. **Objective-C** — `src/Systems/Objective-C/interpreter.m`: expression protocol + recursive objects.
9. **Nim** — `src/Niche/Nim/interpreter.nim`: recursive discriminated expression tree.
10. **Perl** — `src/Scripting/Perl/interpreter.pl`: hash AST + recursive evaluator.
11. **GDScript** — `src/Niche/GDScript/interpreter.gd`: expression objects + recursive interpretation.
12. **Assembly** — `src/LowLevel/Assembly/interpreter.asm`: compact grammar bytecode + explicit opcode interpreter.
13. **Delphi** — `src/Enterprise/Delphi/InterpreterExample.pas`: `IExpression` + number/add expression nodes.
14. **MicroPython** — `src/Other/MicroPython/interpreter.py`: lightweight recursive expression objects.
15. **Rockstar** — `src/Other/Rockstar/interpreter.rock`: expression functions composing number/add evaluation.
16. **VBA** — `src/Shell/VBA/InterpreterExample.bas`: tokenized `Number ('+' Number)*` grammar + evaluator.
17. **Python** — `src/Scripting/PythonPY/patterns/interpreter.py`: tuple-based AST + recursive evaluator.

The Python gap was discovered during final link reconciliation. Its historical `src/Scripting/PythonPY/pattern_sweep.py` contained a correct Interpreter check, but KB-006 explicitly says a multi-pattern runner cannot substitute for an individually addressable canonical `pattern × language` artifact. The runner now orchestrates the extracted Python source instead of owning a second hidden Interpreter implementation.

The audit also corrected false gaps caused by naming/layout differences. Scala and Fortran already had canonical Interpreter sources and duplicate close-out files were removed rather than retained.

## Validation

`Pattern Interpreter Final` is the horizontal certification boundary for close-out artifacts added during this reconciliation. It performs the strongest practical lightweight evidence by ecosystem: format/analyze/compile/runtime where the runtime is available, source-contract validation for VBA/Delphi where hosted Linux lacks the proprietary runtime/compiler, and Python bytecode compilation + standalone execution + aggregate runner execution for the extracted Python cell.

The common close-out teaching contract is a small expression language with a deterministic result; each executable artifact fails or exits non-zero on a wrong result. Existing cells may use a different tiny expression while preserving the same Interpreter intent.

The previously reviewed head `e28f34a94a7468b6cc300e14cc588182aa8bde12` was green before the final Python addressability audit. The current reviewed head must be recertified; predecessor green is not reused as evidence for the newly extracted Python cell or its orchestration change.

## Remaining reconciliation

Before Interpreter can be marked `validated`:

1. certify the Python extraction and all close-out artifacts on the reviewed head;
2. reconcile `wiki/Interpreter.md` with the full 49 Applicable / 2 N/A table, problem, forces, intent, trade-offs, use/no-use guidance, relationships/confusions, Mermaid, verification and factual `En Genkidama` usage;
3. validate links/Mermaid/page structure and generic CI on the resulting documentation head; `Pattern Interpreter Final` now watches `wiki/Interpreter.md` so that documentation changes are recertified;
4. re-read `dev`, reviews and mergeability before promotion readiness.

No aggregate code/test coverage percentage is claimed for this polyglot set. Where percentage coverage is meaningful the repository policy applies; otherwise compile/analyze/runtime evidence is preferred and no number is invented.
