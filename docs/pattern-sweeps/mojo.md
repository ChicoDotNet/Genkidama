# Mojo Design Pattern matrix sweep

> **Target:** Mojo  
> **Target position:** 52nd maintained Genkidama Design Pattern language target  
> **State:** calibration slice in progress  
> **Universe:** 52 catalog patterns  
> **Iteration 1:** 4 canonical cells materialized; 48 remain  
> **Applicability hypothesis:** 52 Applicable / 0 N/A, subject to implementation evidence  
> **Promotion boundary:** this ledger records the Mojo column only; no pattern becomes complete merely because its Mojo cell is green.

## Why Mojo enters as target 52

Mojo is added as a first-class Design Pattern target rather than as a documentation-only experiment. KB-006 therefore applies normally: each Applicable pattern must eventually own an individually addressable canonical Mojo source and behavioral verification.

The first slice deliberately probes four different language forces before scaling the column:

| Family | Pattern | Canonical source | Test | What it probes |
|---|---|---|---|---|
| Structural | Adapter | [`adapter.mojo`](../../src/Systems/Mojo/patterns/adapter.mojo) | [`test_adapter.mojo`](../../src/Systems/Mojo/tests/test_adapter.mojo) | value structs and composition |
| Behavioral | Strategy | [`strategy.mojo`](../../src/Systems/Mojo/patterns/strategy.mojo) | [`test_strategy.mojo`](../../src/Systems/Mojo/tests/test_strategy.mojo) | runtime thin function values |
| Architectural | Microkernel | [`microkernel.mojo`](../../src/Systems/Mojo/patterns/microkernel.mojo) | [`test_microkernel.mojo`](../../src/Systems/Mojo/tests/test_microkernel.mojo) | named plugin registry and failure behavior |
| Concurrency | Monitor Object | [`monitor_object.mojo`](../../src/Systems/Mojo/patterns/monitor_object.mojo) | [`test_monitor_object.mojo`](../../src/Systems/Mojo/tests/test_monitor_object.mojo) | guarded mutable state with a native lock |

## Toolchain and verification

The target pins Mojo **1.1.0**, the current stable release when this slice was authored, in [`pixi.toml`](../../src/Systems/Mojo/pixi.toml). Mojo owns a dedicated Polyglot runtime family so a Mojo-only change does not pay the Haskell/Crystal/Zig/Julia/Objective-C/Nim setup cost and cannot be masked by an unrelated long-tail provisioning failure.

For this calibration slice the gate must:

1. report the resolved Mojo version;
2. require exactly the four expected canonical sources and four matching tests;
3. execute every test file with Mojo's native `TestSuite`;
4. execute [`pattern_sweep.mojo`](../../src/Systems/Mojo/pattern_sweep.mojo);
5. require the sentinel `mojo-pattern-sweep: 4/52 calibration passed`.

The aggregate runner is orchestration only. The four canonical source files remain the individually addressable teaching artifacts.

## Remaining column

The remaining 48 catalog cells stay unimplemented in this iteration. No `N/A` is claimed from absence. The next iterations expand this same target-major slice while preserving native Mojo idioms and the single amortized toolchain context.

## Coverage

No synthetic line-coverage percentage is assigned to this standalone calibration slice. Native compilation through the tests, behavioral assertions, explicit failure-path testing for Microkernel, and aggregate execution are the stronger practical evidence. The repository-wide >=44% rule remains unchanged where meaningful line coverage exists.

`stable for promotion: no` until the planned Mojo column increment is green on its reviewed head and reconciled with current `dev`.
