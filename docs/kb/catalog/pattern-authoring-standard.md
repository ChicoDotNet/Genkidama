# Canonical Design Pattern Authoring Standard

> **Status:** Approved by the owner on 2026-08-14; language-major matrix scheduling and CI-amortization approved on 2026-08-27; canonical-cell and current-toolchain rules approved on 2026-08-28  
> **Applies to:** pattern pages under `wiki/`, their executable examples and approved pattern-sweep ledgers  
> **Catalog:** [`wiki/README.md`](../../../wiki/README.md)  
> **Guiding principle:** architecture comes first; a pattern exists to explain a real design force, not to force the pattern into production code.

## Purpose

KB-006 defines the canonical structure and Definition of Done for every Genkidama Design Pattern. A completed pattern must let a reader determine the problem pressure, trade-off, mechanics, recognition signals, rejection signals, neighboring patterns and idiomatic expression across every target where the intent is meaningful.

The catalog is a connected, executable design vocabulary rather than a collection of definitions or syntax translations.

## Two different kinds of completeness

### Language implementation completeness

Every current Genkidama target must be classified `Applicable` or `N/A`. A pattern reaches `validated` only when **every Applicable target has at least one real, addressable, linked and verified canonical example** and the final pattern page reconciles those links.

A multi-pattern sweep/runner is not that canonical example. It may orchestrate the examples and amortize CI, but it cannot substitute for an individually addressable `pattern × language` artifact.

### Test/code coverage

When meaningful coverage tooling exists:

- **44% is a sufficient minimum floor** when relevant behavior, contracts, failure modes and regressions are protected;
- **44%–72.8% is fully acceptable**;
- coverage above **72.8% is welcome** and must never be penalized;
- **100% is not required**;
- never add low-value tests merely to move a percentage.

Coverage is evidence, not the product goal. Standalone polyglot examples may legitimately report coverage `N/A` when native compile/analyze/runtime evidence is the stronger practical signal.

## Authoring principles

### 1. Problem before pattern

Explain the concrete design problem before implementation mechanics. The reader should understand why the pattern exists before seeing its structure.

### 2. Intent over ceremony

Preserve the pattern's intent. Related patterns may collaborate without being interchangeable definitions.

### 3. Forces and trade-offs are mandatory

Name the competing forces. Pair benefits with costs and failure modes.

### 4. Patterns are not prescriptions

Explain when **not** to use the pattern and point to simpler or better-fit alternatives.

### 5. Relationships are first-class

Use the global relationship map and the labels **collaborates with**, **often implemented with**, **alternative to**, **specializes / generalizes**, and **often confused with** when they accurately describe intent-level relationships.

### 6. Diagrams serve the idea

Every completed page contains at least one fit-for-purpose GitHub Mermaid diagram. Do not force class diagrams where sequence, state, flow or another representation teaches the idea better.

### 7. Examples are executable evidence

A pattern is not complete because its prose is complete. Every Applicable language requires a verified example. Functional modules, closures, algebraic data types, messages, predicates, macros, prototypes, records, traits, interfaces, processes and other native mechanisms are valid when they preserve intent.

A language is not `N/A` merely because it lacks textbook OOP machinery.

### 8. Examples are idiomatic, not transliterations

Use reasonable native idioms. Avoid mechanical ports whose only purpose is reproducing another language's surface shape. Use the strongest lightweight build/compile/parse/run validation reasonably available.

### 9. Every Applicable matrix cell owns an addressable canonical source artifact

For each `pattern × language` cell classified Applicable, the repository must contain at least one stable source artifact whose **primary teaching responsibility is that cell**.

A canonical artifact may be a source file, module, unit, copybook or another ecosystem-native source unit. It must be directly linkable from the pattern page or authoritative sweep ledger.

A file that contains implementations for many unrelated patterns does **not** satisfy this requirement by itself. In particular:

- `pattern_sweep.*`, `PatternSweep.*` and equivalent runners may import/include/load/compile/execute canonical artifacts;
- runners may aggregate assertions and preserve a single expensive runtime setup;
- runners must not hide duplicate canonical implementations once extraction is complete;
- a green runner proves orchestration/aggregate behavior, but it does not make an otherwise unaddressable cell canonical;
- temporary monolithic staging is allowed only while a bounded sweep remains explicitly `in-progress`; it must be split before that slice is promotable.

This rule preserves both educational addressability and CI amortization.

### 10. Repository links must be real

Never use `#`, invented paths, fake availability or speculative links. If an Applicable target lacks its canonical source, the cell remains incomplete.

During an approved language-major sweep, an authoritative ledger under `docs/pattern-sweeps/` may carry the current links until each final pattern page is reconciled.

### 11. Genkidama usage must be factual

`En Genkidama` must either link to a real deliberate use and explain why it fits, or state honestly that Genkidama does not currently use the pattern deliberately. Never distort production architecture to showcase a pattern.

### 12. Tests describe behavior

Verification protects the behavior or variation promised by the pattern, not merely names, inheritance trees or tautological literals.

### 13. Teach distinction

Every completed page addresses at least one misuse, confusion or over-engineering risk.

### 14. CI certifies coherent work; it does not define the smallest work unit

CI certifies meaningful implementation boundaries. It must not become the dominant consumer of execution time because work was divided into artificial micro-pushes.

When changes share owner, target runtime/toolchain, module or validation context and marginal continuation cost is low, finish the coherent slice before paying another expensive setup. Evaluate delivery efficiency by **useful functional work and real debt removed per CI cycle**.

This changes batching, never the quality bar. When execution is genuinely needed to isolate a defect, use CI instead of guessing.

### 15. Pattern CI uses the current stable/LTS toolchain by default

Every pattern validation context must resolve the **most recent stable or LTS toolchain reasonably available** at the time of validation unless a repository-documented incompatibility requires otherwise.

- Prefer official `stable`, `latest` or current LTS channels.
- Do not keep an old compiler merely because an older workflow happened to be green.
- GitHub Actions used by the gate must themselves avoid deprecated embedded runtimes when a maintained alternative exists.
- If the ecosystem's maintained action still depends on a deprecated runtime, install the official stable toolchain directly when practical.
- Verify downloaded release integrity when the ecosystem publishes trustworthy checksums and direct installation is used.
- Do not configure caches whose required manifest/input is absent.
- Record the resolved version in CI output when practical.

A toolchain limitation may be documented; it may not be silently converted into weaker evidence.

## Canonical page structure

Use these headings in this order. A completed page must not leave a required section empty.

~~~markdown
# {Pattern Name}

> **Familia:** {Creational | Structural | Behavioral | Architectural | Integration | Concurrency | Distribution | Presentation | Persistence | Additional}  
> **Intención:** {one-sentence intent}  
> **Estado:** `{in-progress | validated}`  
> **Implementaciones de lenguaje:** `{implemented}/{applicable}`  
> **Cobertura de pruebas:** `{percentage when meaningful, or N/A with reason}`  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase
{Plain-language explanation.}

## El problema
{Concrete situation and design pressure before the solution.}

## Fuerzas que compiten
- {Force 1}
- {Force 2}
- {Force 3}

## La solución
{Intent and core design move.}

## Participantes y responsabilidades
| Participante | Responsabilidad |
|---|---|
| `{Role}` | {Responsibility} |

## Cómo funciona
1. {Step 1}
2. {Step 2}
3. {Step 3}

## Diagrama
```mermaid
{Fit-for-purpose Mermaid diagram.}
```
{Explain what matters in the diagram.}

## Ejemplo mínimo
{Smallest example that preserves intent.}

## Aplicación real
### {Scenario}
{Why the pattern fits, what it buys, and what a simpler alternative would look like.}

## En Genkidama
{Verified deliberate usage, or an honest statement that there is no deliberate production use yet.}

## Cuándo usarlo
- {Signal 1}
- {Signal 2}
- {Signal 3}

## Cuándo no usarlo
- {Simpler alternative 1}
- {Over-engineering risk 2}
- {Better-fit context 3}

## Consecuencias y trade-offs
| A favor | Costo / riesgo |
|---|---|
| {Benefit} | {Cost} |

## Patrones relacionados
[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [{Related Pattern}]({RelatedPattern}.md) | {relationship label} | {Intent-level explanation} |

## Errores comunes y confusiones
### {Confusion or misuse}
{Why it is tempting and how to distinguish it.}

## Cómo comprobar una implementación
- {Observable behavior}
- {Substitution or variation}
- {Failure mode or edge case}

## Implementaciones por lenguaje
The table is authoritative for final language completeness.

| Lenguaje | Aplicabilidad | Ejemplo verificado | Validación | Nota |
|---|---|---|---|---|
| {Language} | Applicable | [{file}]({verified relative path}) | {build/test/run/parse evidence} | {idiomatic mechanism} |
| {Language} | N/A | — | — | {technical justification} |

## Comprueba que lo entendiste
1. {Problem-based selection question}
2. {Comparison with related pattern}
3. {Trade-off or rejection question}

## Resumen
- {Problem pressure}
- {Core move}
- {Main trade-off}
- {Important relationship}
- {Language portability insight}

## Referencias
- {Primary or authoritative pattern reference}
- {Repository architecture/philosophy reference when relevant}
~~~

## Language applicability rules

Build an explicit inventory from every currently maintained Genkidama target. Every target is ultimately `Applicable` or `N/A`.

### What counts as an implementation

An Applicable example must preserve intent, use a reasonable native idiom, live at a stable canonical path, be linkable, receive the strongest practical lightweight validation and avoid infrastructure disproportionate to its teaching goal.

Different targets may use different scenarios. Conceptual equivalence matters more than identical syntax.

### What counts as `N/A`

`N/A` requires a technical explanation of why the intent itself cannot be represented meaningfully in that target/runtime/paradigm. Lack of classes, inheritance or a textbook implementation mechanism is insufficient when functions, modules, closures, messages, prototypes, algebraic types, traits, predicates, macros or other native mechanisms can preserve intent.

When uncertain, keep the target Applicable until review demonstrates otherwise.

## Validation and test coverage policy

Never claim validation that was not executed. If a compiler/runtime cannot reasonably run in CI, document the strongest available static/structural evidence and the limitation; missing tooling does not erase the example requirement.

For expensive runtimes, prefer one validation job that installs/restores once, executes all coherent cells in the slice, fails if any fails and records useful timing evidence when practical. Do not intentionally pay the same expensive setup once per pattern when the cells can safely share the context.

If a batched run finds scoped defects, repair that debt before expanding. Coalesce related fixes when doing so does not hide risk. Conversely, when execution is required to isolate the defect, pay CI rather than continuing blind.

## Scheduling and delivery order

Outside an explicit owner-approved exception, the default remains **one incomplete pattern at a time**.

During a matrix/language scheduling experiment recorded in `docs/roadmap.md`, one branch/PR may own a language-major or multi-runtime cohort spanning many incomplete patterns. Such a sweep may install a slow toolchain once and implement/verify as many remaining Applicable cells as safely fit that context.

The exception changes scheduling and PR ownership only. It does not weaken applicability, idiomaticity, canonical-source addressability, behavioral evidence, coverage policy, Mermaid, relationships, factual `En Genkidama`, link integrity, comprehension requirements or final pattern-page reconciliation.

A partial cell or partial pattern remains `in-progress`. `N/A` is never a scheduling shortcut. The experiment must stay reversible without invalidating correct examples.

## Content rules

Pattern explanations are authored in Spanish. Code identifiers and canonical pattern names remain conventional. There is no word-count quota; prefer the shortest page that completely satisfies the DoD. Summarize references in original wording and respect copyright constraints. The page and [`wiki/README.md`](../../../wiki/README.md) must not contradict each other.

## Definition of Done for one pattern

A pattern is complete only when **all** of the following are true:

- [ ] Canonical name and family are correct.
- [ ] One-sentence intent is accurate and distinct from neighboring patterns.
- [ ] The problem is explained before implementation mechanics.
- [ ] At least three meaningful design forces are named.
- [ ] The solution explains intent, not merely class structure.
- [ ] Participants or an equivalent responsibility model are documented.
- [ ] Interaction or decision flow is explained.
- [ ] At least one fit-for-purpose Mermaid diagram renders.
- [ ] A minimal conceptual example is clearly explained.
- [ ] At least one realistic application explains why the pattern fits.
- [ ] Genkidama usage is verified or explicitly stated as not currently deliberate.
- [ ] `Cuándo usarlo` and `Cuándo no usarlo` are substantive.
- [ ] Benefits and costs are paired as trade-offs.
- [ ] Important related patterns are linked and distinguished by intent.
- [ ] At least one misuse, confusion or over-engineering risk is addressed.
- [ ] Verification guidance describes observable behavior.
- [ ] Every current Genkidama target is classified `Applicable` or `N/A`.
- [ ] Every `N/A` has a defensible technical justification.
- [ ] Every Applicable target has at least one **individually addressable canonical source artifact** for this pattern.
- [ ] A multi-pattern sweep/runner is used only as orchestration and does not substitute for, or duplicate, canonical implementations.
- [ ] Every implementation link resolves; no `#` placeholder or speculative path exists.
- [ ] Each Applicable example has the strongest reasonable validation evidence available.
- [ ] Pattern CI uses the latest stable/LTS toolchain reasonably available, or documents a concrete compatibility exception.
- [ ] GitHub Actions/runtime plumbing contains no known avoidable deprecated-runtime debt.
- [ ] Where meaningful coverage tooling exists, coverage is at least **44%** or a concrete repository-wide exception is documented.
- [ ] Coverage above 72.8% is accepted without penalty; 100% is not required.
- [ ] Three comprehension questions require reasoning.
- [ ] References are present where appropriate and copyright constraints are respected.
- [ ] No `TODO`, `TBD`, `PLACEHOLDER`, empty heading or knowingly speculative claim remains.
- [ ] Markdown links, Mermaid syntax and the final language table have been reviewed.

Any failed mandatory item leaves the pattern **in progress**.

## Review rubric

| Dimension | Weight | Review question |
|---|---:|---|
| Correctness of intent and distinctions | 25% | Would an experienced engineer recognize the pattern and its boundaries? |
| Cross-language correctness and idiomaticity | 25% | Do implementations preserve intent without forcing one paradigm onto every language? |
| Decision usefulness | 15% | Can the reader decide when to use or reject it? |
| Relationships and composition | 10% | Does it connect accurately to the surrounding vocabulary? |
| Examples and verification evidence | 15% | Are examples real, individually addressable, linked and proportionately validated? |
| Navigation and references | 10% | Is the page easy to navigate and substantiate? |

Correctness is non-compensable. A materially wrong definition or a non-idiomatic example presented as canonical fails review even if every file exists.

## Agent workflow

1. Read this standard, `docs/roadmap.md`, the relevant ledger/page, actual example files and the relevant relationship-map neighborhood.
2. Repay bounded debt in the active owner/slice before expanding it.
3. Outside an active exception, work one pattern at a time; under an approved matrix experiment, work the authorized target/cohort.
4. While the same runtime/toolchain/context is loaded and marginal cost is low, continue coherent cells instead of publishing artificial micro-increments.
5. Prefer staging a coherent slice before moving the review ref, so one publication triggers one expensive certification when practical.
6. Never call a materialized-but-unrun cell verified.
7. Maintain the sweep ledger during matrix work and reconcile final pattern pages before any pattern becomes `validated`.
8. Verify canonical paths and applicability; never infer completion from naming conventions or from a green monolithic runner.
9. Use current stable/LTS toolchains and inspect resolved versions/deprecated-action warnings.
10. Run the strongest practical target validation plus applicable repository checks and inspect the final diff.
11. Once relevant behavior is protected and meaningful coverage is >=44%, do not stall merely to chase 72.8%, 90% or 100%.
12. Do not mix course implementation, runtime refactors or unrelated product work into a pattern-sweep PR.

## Approved rollout

1. [`AbstractFactory.md`](../../../wiki/AbstractFactory.md) remains the golden reference for final pattern-page quality.
2. Completed pattern artifacts keep the same full DoD regardless of scheduling strategy.
3. An active owner-approved language-major experiment may traverse the matrix by target runtime or coherent runtime cohort to amortize CI.
4. The active experiment order and review point live in `docs/roadmap.md`.
5. Coordinate with the Learn lane through the unified roadmap: each lane spends roughly 80% of effort on its own delivery and 20% checking compatibility with the other lane.
