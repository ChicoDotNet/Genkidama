# Genkidama Roadmap

This document is the **authoritative program roadmap** for Genkidama.

Detailed lane state lives in the lane-specific sources linked below, but no lane may treat its local roadmap as independent from this document.

## Branch strategy

- Feature work: dedicated reviewable branches.
- Integration: `dev`.
- Stable releases: `main`.
- Autonomous agents never merge and never enable auto-merge.
- The owner promotes `dev -> main` when the active lanes report a simultaneously stable integration state.

## Program model

Genkidama evolves as one product with coordinated workstreams, not as isolated subprojects.

The intended knowledge flow remains:

```text
Knowledge Base -> Templates -> CLI -> Generated Applications
```

The current autonomous educational program uses **two coordinated lanes** on the same `dev` integration branch:

```text
                         +----------------------+
                         |         dev          |
                         | integration truth    |
                         +----------+-----------+
                                    |
                    owner promotes when both stable
                                    |
                                    v
                         +----------------------+
                         |        main          |
                         | stable releases      |
                         +----------------------+

       Course lane                               Pattern lane
  80% 0 -> Junior                          80% Design Pattern catalog
  20% cross-lane safety                    20% cross-lane safety
          \                                      /
           +---------------> dev <---------------+
```

## 80/20 coordination contract

Each lane spends approximately:

- **80%** of its effort advancing its own highest-value incomplete work;
- **20%** verifying that its proposed changes do not break, regress, contradict or invalidate the other lane.

The 20% allocation is a compatibility responsibility, not permission to duplicate the other lane's backlog.

Before publishing an increment, each lane must inspect the current `dev`, the other lane's active PR/state, shared files and applicable CI. If `dev` moved while work was in progress, reconcile before declaring stability.

### Stable-for-promotion handshake

A lane may report **stable for promotion** only when:

1. its own current increment is complete for the intended promotable scope;
2. its applicable checks are green, or a documented platform limitation is explicitly outside the change and does not hide a known defect;
3. the branch/PR is mergeable against the current `dev`;
4. no known regression is introduced into the other lane;
5. shared roadmap/state documents are internally consistent;
6. no bounded debt introduced by the lane remains unpaid;
7. the lane has inspected the other lane's current integration surface rather than assuming compatibility.

A matrix slice that intentionally leaves its patterns incomplete may be a **complete experiment increment** without being **stable for promotion**. Pattern promotion still requires the full KB-006 pattern DoD.

`main` promotion is a human synchronization point. Agents do not infer permission to merge from both lanes being stable.

## Shared code-coverage policy

When meaningful code/test coverage tooling exists:

- **44% is sufficient as the approval floor** when relevant behavior, contracts, failure modes and regressions are protected;
- **44%–72.8% is a fully acceptable range**;
- coverage above **72.8% is welcome** and must never be penalized;
- **100% code/test coverage is not required**;
- a lane must not stall valuable delivery merely to chase a higher percentage once the applicable quality gates are satisfied.

Coverage is evidence, not the product goal. Do not manufacture low-value tests to optimize the metric.

## Shared CI-efficiency policy

CI certifies progress; it must not fragment development into economically tiny units.

For each delivery boundary, optimize for **useful functional work and real debt removed per full validation cycle** while keeping the same correctness bar. In particular:

- when several changes share one owner, feature, module, language runtime or toolchain, finish the coherent batch while marginal implementation cost is low;
- avoid repeated pushes that reinstall the same expensive runtime merely to certify one small variation at a time;
- prefer one setup followed by many behavioral checks when those checks can safely share the runtime;
- keep defects visible and repay discovered scoped debt before moving to a different owner/slice;
- do not weaken validation, skip required checks or claim unexecuted evidence to improve speed;
- use observed setup + validation latency and completed functional cells to decide future batching boundaries.

The desired direction is that each successive matrix increment removes **more remaining work per expensive CI cycle**, especially while high-overhead target runtimes remain.

## Lane A — 0 -> Junior courses

### Mission

Build practical Spanish-first courses that take a learner from zero knowledge of a language to a reasonable Junior Developer / Entry Level foundation without promising employment.

### Sources of truth

- [`learn/_meta/progress.yml`](../learn/_meta/progress.yml)
- [`learn/_meta/roadmap.md`](../learn/_meta/roadmap.md)
- [`learn/_meta/course-spec.md`](../learn/_meta/course-spec.md)
- [`learn/_meta/pedagogy.md`](../learn/_meta/pedagogy.md)
- [`learn/_meta/decisions.md`](../learn/_meta/decisions.md)

### Primary work

- Finish one incomplete course before opening another unless the repository records a justified exception.
- Preserve the established 0 -> Junior Definition of Done.
- Keep Git as a transversal course outside the 45-language denominator.
- Integrate course PRs into `dev`, not directly into `main`.

### 20% cross-lane responsibility

Before declaring a course increment stable, check at minimum:

- no catalog/Knowledge Base paths are deleted or reverted;
- shared README/roadmap/navigation remains coherent;
- the course does not teach a Design Pattern inaccurately or contradict the approved pattern standard;
- changes to shared validation/workflows do not disable pattern-catalog checks.

## Lane B — Design Pattern catalog

### Mission

Turn the Design Pattern catalog into a connected and executable learning reference under the approved canonical standard. The default cadence completes one pattern at a time; the active owner-approved experiment traverses expensive portions of the pattern/language matrix **by target language/runtime** to amortize CI without weakening the Definition of Done.

### Sources of truth

- [`wiki/README.md`](../wiki/README.md)
- [`docs/kb/catalog/pattern-authoring-standard.md`](kb/catalog/pattern-authoring-standard.md)
- [`docs/philosophy/001-patterns-as-living-examples.md`](philosophy/001-patterns-as-living-examples.md)
- target sweep ledgers under [`docs/pattern-sweeps/`](pattern-sweeps/), while their slices are incomplete
- this roadmap.

### Primary work

1. Keep `AbstractFactory.md` as the golden reference for final pattern-page quality.
2. Keep full Applicable-language completeness as the completion gate for every pattern, regardless of scheduling order.
3. Preserve completed Creational/Structural work and continue the catalog relationships family by family at final reconciliation time.
4. During the active experiment, let the **target language/runtime** own a coherent slice spanning all remaining patterns when that materially amortizes CI.
5. After the expensive target rows are substantially retired, complete horizontal pattern pages/remaining cheap cells in the order that maximizes useful progress per validation cycle.

A pattern is not complete until **every language in which the pattern can be implemented meaningfully has a verified example**. This is language-set completeness, not a demand for 100% code/test coverage. `N/A` requires technical justification and review; lack of classes/OOP syntax is not enough.

### Owner-approved matrix scheduling experiment — language-major, expensive targets first

The owner first approved the matrix experiment after Chain of Responsibility was integrated into `dev`, then clarified its intended granularity on 2026-08-27: **a slow/high-overhead language should cover all remaining patterns in one coherent implementation/CI slice whenever practical**.

The Command-only MATLAB pass is retained as a pilot measurement, not as the desired delivery granularity. It observed approximately **92 s of MATLAB setup versus 6 s of Command validation**. That evidence shows that repeating setup per pattern would make CI the dominant cost.

The experiment starts from the catalog state of **39 patterns remaining after Chain of Responsibility** and **51 language targets**.

#### Phase 1 — MATLAB column in one slice

1. Decide MATLAB applicability for all 39 remaining patterns.
2. Materialize every MATLAB-Applicable pattern example before publishing the target validation boundary.
3. Use one target-level validator to execute all 39 examples in the same MATLAB runtime.
4. Install/setup MATLAB once for the sweep and record `cells`, `setup_seconds`, `validation_seconds` and `total_seconds`.
5. Track the partial cells in [`docs/pattern-sweeps/matlab.md`](pattern-sweeps/matlab.md).
6. Keep every incomplete canonical pattern `in-progress`; the green MATLAB sweep certifies the MATLAB column only.

#### Phase 2 — other high-overhead target columns

After MATLAB, rank the next target primarily from **observed repository CI cost and remaining cell count**, not from theoretical compiler speed alone.

Candidates explicitly worth inspecting early include runtimes/toolchains such as **Haskell, Dart and Crystal**, along with any other target whose setup/validation history shows material fixed cost.

For the selected target:

1. open/reuse one language-major sweep branch/PR;
2. implement that target across all remaining Applicable patterns while the context/toolchain remains coherent;
3. validate the whole target slice after one setup when the ecosystem permits;
4. record timing and completed-cell evidence;
5. repay failures/debt discovered by that gate before switching targets.

#### Phase 3 — progressively cheaper rows and horizontal closure

Continue language-major sweeps while they remove more matrix work per CI cycle than pattern-major execution.

As fixed setup cost falls, or as the remaining matrix becomes sparse, switch back toward horizontal pattern completion whenever that produces the better coherent validation unit. The experiment is an optimization strategy, not a permanent obligation to stay vertical.

Re-evaluate batching using evidence such as:

- runtime/toolchain setup latency;
- validation latency;
- number of remaining Applicable cells for that target;
- amount of bounded debt retired;
- probability that another push would merely repeat already-paid setup context.

### Operational rules for the experiment

- **One active PR per partial pattern is no longer required during an approved language-major sweep.** The target-language slice may be owned by one branch/PR across many patterns.
- Outside the active matrix exception, the normal one-pattern-at-a-time / pattern-owned PR rule resumes.
- Prefer staging a coherent target slice before moving the PR ref when practical, so one publication triggers one expensive target certification rather than dozens of nearly identical cycles.
- A target ledger under `docs/pattern-sweeps/` is authoritative for partial matrix-cell state until each canonical pattern page is reconciled for final validation.
- A partial language cell does **not** make its pattern `validated`, complete, stable for promotion or roadmap-complete. Those claims still require `implemented == applicable` and every other KB-006 gate.
- Draft sweep branches must reconcile current `dev` before any stability claim; valid work from the course lane must be preserved.
- Partial work must remain factual: no fake implementation links, no speculative `N/A`, no invented CI evidence and no production architecture changes merely to showcase a pattern.
- A materialized cell is not called verified until the required target gate has actually passed on the reviewed head.
- When lightweight instrumentation is practical, record setup/validation/total timing. Prefer medians across comparable green runs for ranking later targets.
- Timing telemetry never weakens correctness checks and never justifies skipping a stronger reasonable validation.
- If a batched gate fails, fix the scoped slice before expanding to another target; coalesce related fixes into the next meaningful validation boundary when safe.
- The experiment is reversible. Removing it restores the default pattern-major scheduling rule without invalidating correct examples already produced.

### 20% cross-lane responsibility

Before declaring a pattern/sweep increment stable, check at minimum:

- no `learn/**` material or course metadata is deleted or reverted;
- shared README/roadmap/navigation remains coherent;
- example code does not silently mutate course applications or course-specific source trees;
- shared workflows/validation remain compatible with Learn;
- a pattern example may reference a course language, but must not hijack the course's pedagogical scope.

## Shared conflict rules

When both lanes need the same shared file:

1. re-read current `dev` immediately before writing;
2. preserve both lanes' valid intent semantically;
3. prefer additive changes over replacing a shared file with a historical copy;
4. do not resolve a conflict by reverting the other lane;
5. re-run the checks affected by the shared file;
6. record any intentional cross-lane decision in the appropriate source of truth.

If one lane discovers bounded debt in shared infrastructure caused by its own change, it repays that debt before expanding its own backlog.

## Engineering Core backlog

The Engineering Core remains part of the same product roadmap. Educational lanes must not regress it.

- **GEN-000** — repository foundation, governance and purpose.
- **GEN-001** — CLI bootstrap, engineering standards, MSTest, CI and documentation structure.
- **GEN-002** — `genkidama new` solution generator.
- **GEN-003** — StandardResult, StandardCollectionResult, StandardQuery, StandardProblem.
- **GEN-004** — HTTP layer, HttpQuery attribute, middleware, trace identifiers.
- **GEN-005** — persistence factory and supported providers.
- **GEN-006** — Repository and Unit of Work.
- **GEN-007** — Command/Query pipeline.
- **GEN-008** — StandardJob and background processing.
- **GEN-009** — StandardEvent and notification pipeline.
- **GEN-010** — security foundation.
- **GEN-011** — React reference client.
- **GEN-012** — Angular reference client.
- **GEN-013** — Console reference client.
- **GEN-014** — WinForms MVP reference client.
- **GEN-015** — MAUI MVVM reference client.
- **GEN-016** — `genkidama add entity`.
- **GEN-017** — `genkidama add enum` and enum schema seed support.
- **GEN-018** — `genkidama add feature` vertical slice generator.
- **GEN-019** — `genkidama add module` for cross-cutting modules.
- **GEN-020** — template system, plugins and provider extension points.
- **GEN-021** — testing hardening, contracts, integration and coverage.
- **GEN-022** — packaging, .NET Tool publication and release automation.
- **GEN-023** — localization foundation.
- **GEN-024** — Design Patterns Example App localization.
- **GEN-025** — documentation localization pipeline.

## Knowledge Base state

Current numbered KB deliveries are recorded in [`docs/kb/README.md`](kb/README.md). That index is authoritative for KB identifiers; do not reuse an existing KB number for a different topic.

Current direction includes:

- engineering standards and delivery checks;
- architecture references and ADRs;
- design principles and philosophy;
- Design Pattern catalog navigation and relationship map;
- approved Design Pattern authoring/implementation standard;
- future stack, persistence, integration, localization, documentation and release references as justified by product work.

## Program Definition of Done

For any increment:

- Builds successfully when code exists and tooling is available.
- Applicable tests pass.
- Measurable code/test coverage follows the shared policy above; 100% is never required.
- Existing quality gates are not weakened to obtain green status.
- Engineering standards are respected.
- Shared roadmap/state is updated when scope or operating policy changes.
- Knowledge Base impact is considered for template or architecture changes.
- Course changes respect the course DoD.
- Pattern changes respect the approved pattern DoD, including an example for every Applicable language before `validated`.
- Matrix-sweep changes respect the target ledger and do not overstate partial pattern completion.
- The other active lane has been checked for compatibility before an increment is reported stable.

## Promotion rhythm

The normal integration rhythm is:

```text
coherent lane increment -> PR to dev -> reconcile -> checks -> lane stable when promotable
other lane increment    -> PR to dev -> reconcile -> checks -> lane stable when promotable
both lanes stable on compatible dev -> owner may promote dev to main
```

For an intentionally partial matrix sweep, a green target slice may immediately lead to the next target slice without claiming promotion readiness for incomplete patterns.

There is no requirement to wait for a fixed amount of work, nor to publish after an arbitrary number of changed files. The preferred boundary is **maximum coherent functional progress per certification cycle**, followed by simultaneous integration stability when promotion is actually sought.
