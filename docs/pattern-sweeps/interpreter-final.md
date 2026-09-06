# Interpreter — final KB-006 reconciliation

This ledger records factual progress for the horizontal Interpreter close-out. The canonical authority is `wiki/Interpreter.md`; this ledger records how that page and its language cells were certified.

## Baseline

- Base observed for the reconciliation: `dev@57d5d439d1105ef8e380869866a697f0c4ef17bd`.
- KB-006 is approved on `dev`.
- Current target universe: **51 targets**.
- Final applicability inventory: **49 Applicable, 2 N/A**.
- `HTML` and `CSS` are the only N/A targets. HTML can describe syntax-shaped markup but cannot itself execute an evaluator for another grammar; CSS is a browser-evaluated styling rule language and does not provide an author-programmable evaluation mechanism for such a grammar. These exclusions are based on Interpreter intent, not on lack of classes/OOP.
- `wiki/Interpreter.md` now carries the complete KB-006 pattern page and the individually linked 51-target table.

## Canonical-source audit

The complete 49-Applicable inventory has an individually addressable canonical source for Interpreter on this branch. Sweep runners are orchestration/evidence only and are not counted as canonical cells.

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

The predecessor head `b7fa20c345007a2013fcd83d2a875e7b80989e25` completed its applicable workflows green: generic CI, Interpreter Final, Python sweep, Medium-High Cohort and Nim Canonical. That predecessor green is evidence for the implementation state only; it is not reused as certification of the documentation commits that follow.

`Pattern Interpreter Final` watches `wiki/Interpreter.md`, so the final documentation head must execute the horizontal certification again. Promotion is evaluated only from that final head after generic CI, applicable pattern workflows, links/page validation, `dev` reconciliation, reviews and mergeability have been re-read.

## Canonical page

`wiki/Interpreter.md` now records:

- problem, intent and competing forces;
- participants, flow and Mermaid;
- trade-offs and use/no-use guidance;
- relationships and common confusions, including parser ≠ Interpreter;
- factual `En Genkidama`: no deliberate current product use was found, so no architecture was introduced to manufacture one;
- behavioral verification guidance;
- the complete **49/49 Applicable + 2 N/A** implementation table with direct canonical links.

No aggregate code/test coverage percentage is claimed for this polyglot set. Where percentage coverage is meaningful the repository policy applies; otherwise compile/analyze/runtime evidence is preferred and no number is invented.

## Promotion gate

Interpreter is eligible for `stable for promotion: yes` only after the current documentation head is confirmed mergeable against the then-current `dev`, all applicable workflows are green, the canonical links/page checks are green, own debt remains zero, and the 20% Learn/metadata/navigation/workflow compatibility check finds no regression. Until those facts are observed, the PR remains Draft and this ledger makes no premature promotion claim.
