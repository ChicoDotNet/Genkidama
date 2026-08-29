# Interpreter — final KB-006 reconciliation

This ledger records factual progress for the horizontal Interpreter close-out. The canonical pattern page remains `wiki/Interpreter.md`; this file must not be used to claim pattern completion before the full KB-006 page is reconciled.

## Baseline

- Base: `dev@57d5d439d1105ef8e380869866a697f0c4ef17bd`.
- KB-006 is approved on `dev`.
- Current target universe: **51 targets**.
- Final applicability inventory: **49 Applicable, 2 N/A**.
- `HTML` and `CSS` are the only N/A targets. HTML can describe syntax-shaped markup but cannot itself execute an evaluator for another grammar; CSS is a browser-evaluated styling rule language and does not provide an author-programmable evaluation mechanism for such a grammar. These exclusions are based on Interpreter intent, not on lack of classes/OOP.
- `wiki/Interpreter.md` is still historical/incomplete and is not yet a KB-006 completion claim.

## Canonical-source audit

The complete 49-Applicable inventory now has an individually addressable canonical source for Interpreter.

Existing canonical coverage was established through the integrated language-major ledgers and individual target sweeps:

- portable-functional cohort: Rust, Java, C++, C, R, GNU Octave, OCaml, Common Lisp, Elixir, Erlang, Groovy, Prolog and PowerShell;
- medium-high cohort: Scala, Clojure, Kotlin, Swift, C#, Visual Basic .NET, F#, Solidity, Ada, Pascal, COBOL, Fortran, Nim and TypeScript;
- existing individual canonical sweeps/sources: Python, PHP, Ruby, JavaScript, Lua, Bash and MATLAB.

The horizontal audit identified **16 canonical gaps** that were not satisfied by an aggregate runner or previous per-pattern source and materialized them in this PR:

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

The audit also corrected false gaps caused by naming/layout differences. Scala and Fortran already had canonical Interpreter sources and duplicate close-out files were removed rather than retained.

## Validation

`Pattern Interpreter Final` is the horizontal certification boundary for the 16 canonical artifacts added during this close-out. It performs the strongest practical lightweight evidence by ecosystem: format/analyze/compile/runtime where the runtime is available, and source-contract validation for VBA/Delphi where hosted Linux lacks the proprietary runtime/compiler.

The common teaching contract is the grammar/evaluation result `2 + 3 + 4 = 9`; each executable example fails or exits non-zero on a wrong result.

The previously reviewed head `4a060df25293b7426afebabd17e6936b25699ba7` was fully green for CI, Medium-High Cohort, Nim Canonical and Interpreter Final before the seven remaining gaps were added. The current head must be recertified; no predecessor green is reused as evidence for newly added cells.

## Remaining reconciliation

Before Interpreter can be marked `validated`:

1. certify all 16 close-out artifacts on the reviewed head;
2. reconcile `wiki/Interpreter.md` with the full 49 Applicable / 2 N/A table, problem, forces, intent, trade-offs, use/no-use guidance, relationships/confusions, Mermaid, verification and factual `En Genkidama` usage;
3. validate links/Mermaid/page structure and generic CI on the resulting documentation head;
4. re-read `dev`, reviews and mergeability before promotion readiness.

No aggregate code/test coverage percentage is claimed for this polyglot set. Where percentage coverage is meaningful the repository policy applies; otherwise compile/analyze/runtime evidence is preferred and no number is invented.
