# Command final reconciliation

> **Pattern:** Command  
> **Universe:** 51 targets  
> **Applicability:** 49 Applicable, 2 N/A (HTML, CSS)  
> **Previously verified:** 41 Applicable targets through integrated matrix sweeps and MATLAB  
> **Final slice:** 8 Applicable targets: SQL, Perl, GDScript, Assembly, Delphi, MicroPython, Rockstar, VBA  
> **Pattern completion:** all 49 Applicable sources are materialized and the canonical [`wiki/Command.md`](../../wiki/Command.md) table is reconciled; promotion still requires the reviewed PR head to finish green and remain mergeable against current `dev`.

This ledger is retained as provenance for the eight-cell horizontal closure. The canonical pattern page is authoritative for final language completeness.

## Final cells

| Target | Applicability | Canonical source | Validation |
|---|---|---|---|
| SQL | Applicable | [`command.sql`](../../src/Data/SQL/command.sql) | SQLite execution; command rows preserve operation, amount and ordering |
| Perl | Applicable | [`command.pl`](../../src/Scripting/Perl/command.pl) | `perl -c` + runtime behavior |
| GDScript | Applicable | [`command.gd`](../../src/Niche/GDScript/command.gd) | Godot 4.6.3 headless runtime |
| Assembly | Applicable | [`command.asm`](../../src/LowLevel/Assembly/command.asm) | NASM + ld + runtime behavior |
| Delphi | Applicable | [`CommandExample.pas`](../../src/Enterprise/Delphi/CommandExample.pas) | strongest hosted evidence: source contract; DCC unavailable on GitHub-hosted Linux |
| MicroPython | Applicable | [`command.py`](../../src/Other/MicroPython/command.py) | MicroPython 1.28.0 Unix port runtime |
| Rockstar | Applicable | [`command.rock`](../../src/Other/Rockstar/command.rock) | official Rockstar v2.0.31 runtime |
| VBA | Applicable | [`CommandExample.bas`](../../src/Shell/VBA/CommandExample.bas) | strongest hosted evidence: source contract; Office/VBA runtime unavailable on GitHub-hosted Linux |

HTML and CSS remain N/A for Command because pure markup/style rules do not provide an executable request value plus dispatcher/receiver mechanism. This is not based on lack of classes.

## Coverage

Coverage is N/A as an aggregate percentage. These are standalone polyglot examples; native parse/compile/run evidence is stronger than a synthetic common line-coverage denominator. The repository 44% floor remains applicable wherever meaningful instrumentation exists.

## Final evidence boundary

The eight-cell gate passed on the predecessor reviewed head before canonical-page reconciliation. The reconciliation commits change documentation/state only; they do not inherit a green claim automatically. The current PR head must complete its applicable GitHub Actions checks successfully before Command is reported stable for promotion or the PR is moved out of Draft.
