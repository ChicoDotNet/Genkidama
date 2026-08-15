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

1. its own current increment is complete for the intended scope;
2. its applicable checks are green, or a documented platform limitation is explicitly outside the change and does not hide a known defect;
3. the branch/PR is mergeable against the current `dev`;
4. no known regression is introduced into the other lane;
5. shared roadmap/state documents are internally consistent;
6. no bounded debt introduced by the lane remains unpaid;
7. the lane has inspected the other lane's current integration surface rather than assuming compatibility.

`main` promotion is a human synchronization point. Agents do not infer permission to merge from both lanes being stable.

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

Turn the Design Pattern catalog into a connected and executable learning reference, completing one pattern at a time under the approved canonical standard.

### Sources of truth

- [`wiki/README.md`](../wiki/README.md)
- [`docs/kb/catalog/pattern-authoring-standard.md`](kb/catalog/pattern-authoring-standard.md)
- [`docs/philosophy/001-patterns-as-living-examples.md`](philosophy/001-patterns-as-living-examples.md)
- this roadmap.

### Primary work

1. Retrofit `AbstractFactory.md` as the golden reference.
2. Complete its Applicable-language coverage to 100% before moving to the next pattern.
3. Finish the remaining Creational family: Builder, Factory Method, Prototype and Singleton.
4. Continue family by family while keeping the global relationship map coherent.
5. Use one PR per pattern; multiple commits are expected when cross-language coverage is large.

A pattern is not complete until **every language in which the pattern can be implemented meaningfully has a verified example**. `N/A` requires technical justification and review; lack of classes/OOP syntax is not enough.

### 20% cross-lane responsibility

Before declaring a pattern increment stable, check at minimum:

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
- Existing quality gates are not weakened to obtain green status.
- Engineering standards are respected.
- Shared roadmap/state is updated when scope or operating policy changes.
- Knowledge Base impact is considered for template or architecture changes.
- Course changes respect the course DoD.
- Pattern changes respect the approved pattern DoD, including 100% Applicable-language coverage before `validated`.
- The other active lane has been checked for compatibility before the increment is reported stable.

## Promotion rhythm

The expected rhythm is:

```text
lane increment -> PR to dev -> reconcile -> checks -> lane stable
other lane increment -> PR to dev -> reconcile -> checks -> lane stable
both lanes stable on compatible dev -> owner may promote dev to main
```

There is no requirement to wait for a fixed amount of work. The promotion boundary is **simultaneous integration stability**, not a calendar event or arbitrary commit count.
