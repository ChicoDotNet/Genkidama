# Canonical Design Pattern Authoring Standard

> **Status:** Approved by the owner on 2026-08-14; language-major matrix scheduling and CI-amortization rule approved on 2026-08-27  
> **Applies to:** pattern pages under `wiki/`, their executable examples and approved pattern-sweep ledgers  
> **Catalog:** [`wiki/README.md`](../../../wiki/README.md)  
> **Guiding principle:** architecture comes first; a pattern exists in the catalog to explain a real design force, not to force the pattern into production code.

## Purpose

This standard defines the canonical structure and Definition of Done for every Genkidama Design Pattern.

A completed pattern must let a reader answer:

1. What problem creates pressure for this pattern?
2. What trade-off does it make?
3. How does it work and how can I recognize it?
4. When should I choose something simpler or different?
5. Which patterns commonly appear around it?
6. How does the same intent translate idiomatically across every language where the pattern can be implemented?

The catalog is a connected, executable design vocabulary rather than a collection of isolated definitions.

## Two different kinds of completeness

Do not confuse **language implementation completeness** with **test/code coverage**.

### Language implementation completeness

For a pattern to be considered implemented, every current Genkidama language target must be classified as `Applicable` or `N/A`, and **every Applicable language must have at least one real, linked and verified example**.

This is a completeness requirement for the educational catalog. It is not a demand for 100% line, branch or method coverage.

### Test/code coverage

When meaningful coverage tooling exists, the project-wide approval policy applies:

- **44% is a sufficient minimum floor** for approval;
- **44%–72.8% is a fully acceptable coverage range**;
- coverage above **72.8% is welcome and must never be penalized**;
- **100% test/code coverage is not required**;
- do not delay the next valuable increment merely to chase a higher percentage once the relevant behavior is adequately tested and the applicable quality gates pass.

Coverage percentage is evidence, not the product goal. Behavioral confidence, important paths, failure modes and regression protection matter more than maximizing the number.

## Authoring principles

### 1. Problem before pattern

Describe the concrete design problem before implementation mechanics. The reader should understand why the pattern is useful before seeing its structure.

### 2. Intent over ceremony

Explain the pattern's intent accurately. Related patterns may collaborate without being interchangeable definitions.

### 3. Forces and trade-offs are mandatory

Every pattern resolves competing forces. A page that lists benefits without costs is incomplete.

### 4. Patterns are not prescriptions

Every page explains when **not** to use the pattern and points to a simpler or more appropriate alternative where possible.

### 5. Relationships are first-class

Every page links to the global relationship map and explains important relationships using these labels:

- **collaborates with** — patterns solve different parts of the same design;
- **often implemented with** — one commonly supplies a mechanism used by another;
- **alternative to** — similar pressure, different trade-offs;
- **specializes / generalizes** — more specific or broader expression of the idea;
- **often confused with** — similar surface structure, different intent.

### 6. Diagrams serve the idea

Every completed page contains at least one Mermaid diagram using the type that explains the pattern best: `classDiagram`, `sequenceDiagram`, `stateDiagram-v2`, `flowchart`, or another GitHub-supported Mermaid form when justified.

Do not force every pattern into a class diagram.

### 7. Examples are executable evidence

A pattern is not complete merely because its prose is complete. It must have an example in **every language in which the pattern can be implemented meaningfully**.

The example does not need to mimic class-oriented syntax. Functional modules, closures, algebraic data types, message passing, predicates, macros, prototypes, records, traits, interfaces, processes or other native language mechanisms are valid when they preserve the pattern's intent.

A language is **not** `N/A` merely because it lacks classes, inheritance, interfaces or another mechanism used by a textbook example.

For each target language the author must classify applicability as:

- **Applicable** — the pattern's intent can be represented meaningfully and therefore requires a verified example;
- **N/A** — the pattern is genuinely meaningless or impossible in that language/runtime/paradigm, with a concise technical justification.

`N/A` is an exception, not a shortcut. It must survive review.

### 8. Examples are idiomatic, not transliterations

Each language example should express the same design intent using reasonable idioms for that language. Avoid mechanical ports whose only purpose is reproducing the shape of another implementation.

Where tooling permits, examples must build, compile, parse, run or otherwise pass the strongest lightweight automated validation reasonably available.

### 9. Repository links must be real

Never use `#`, invented paths, fake availability or "coming soon" as implementation links. If an applicable language lacks its required example, the pattern remains **in progress**.

During an approved language-major sweep, an incomplete cell may be linked first from an authoritative target ledger under `docs/pattern-sweeps/`. Before the pattern reaches `validated`, its canonical page must reconcile the complete language table and contain the real implementation links itself.

### 10. Genkidama usage must be factual

The **En Genkidama** section must either link to a real deliberate use of the pattern and explain why it fits, or state that Genkidama does not currently use it deliberately.

Never distort production architecture to showcase a pattern.

### 11. Tests describe behavior

Verification should test the behavior or variation promised by the pattern, not merely assert class names or inheritance trees.

Do not chase 100% coverage as a proxy for correctness. Once coverage is at least 44% and the important behavioral risks are protected, additional coverage should be added only when it provides meaningful confidence.

### 12. Teach distinction

Every completed page addresses at least one misuse, confusion or over-engineering risk.

### 13. CI certifies coherent work; it does not define the smallest work unit

CI exists to certify a meaningful implementation boundary. It must not become the dominant consumer of execution time merely because work was divided into artificially small pushes.

When several changes share the same owner, target language/runtime, toolchain, module or validation context and the marginal cost of continuing is low, finish the larger coherent slice before paying another full validation cycle.

Evaluate delivery efficiency by **useful functional work and real debt removed per CI cycle**, not by the number of commits, pushes or green checks produced.

This principle never authorizes weaker checks, speculative validation or delayed defect repair. It changes the **batching boundary**, not the quality bar.

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

For each pattern, build an explicit applicability inventory from the language targets currently maintained by Genkidama. Every target must ultimately be classified `Applicable` or `N/A`.

A pattern reaches `validated` only when every language classified **Applicable** has a verified example satisfying this standard. Express this as a complete set (`implemented == applicable`), not as "100% test coverage".

### What counts as an implementation

A language example must:

1. preserve the pattern's intent rather than merely its class diagram;
2. use a reasonable native idiom for that language;
3. live at a stable repository path;
4. be linked from the final canonical pattern page;
5. have the strongest practical lightweight validation available for that ecosystem;
6. avoid dependencies or infrastructure disproportionate to the teaching goal.

Several languages may use different scenarios if that makes the pattern more natural. Conceptual equivalence matters more than identical examples.

### What counts as `N/A`

`N/A` requires a technical explanation of why the intent itself cannot be represented meaningfully in that language/runtime, not merely why a textbook implementation shape is unavailable.

If the intent can be expressed with functions, modules, messages, closures, prototypes, algebraic types, traits, predicates, macros or other native mechanisms, the language remains **Applicable**.

When uncertain, classify the language as Applicable until review demonstrates otherwise.

## Validation and test coverage policy

Do not claim validation that was not executed. When a compiler/runtime cannot reasonably run in CI, document the best available static or structural evidence and the limitation. Missing tooling does not erase the requirement for the example.

When code coverage can be measured meaningfully:

- 44% or more is sufficient for approval when the relevant behavior, failure modes and contracts are tested;
- 44%–72.8% is a healthy and fully acceptable range;
- more than 72.8% is welcome, especially when it comes naturally from useful tests;
- never add low-value tests merely to increase a percentage;
- never block the next valuable slice because coverage is below 100%; 100% is not a requirement.

### CI amortization

For a runtime/toolchain with meaningful setup cost, prefer one validation job that:

1. installs or restores that target runtime once;
2. executes all coherent cells included in the target slice;
3. fails if any cell fails;
4. records useful timing evidence when practical.

Do not intentionally trigger the same expensive setup once per pattern when those pattern cells can be validated safely in one process/job.

Do not publish a push merely to learn whether each tiny sub-step is green when the same local/static reasoning can safely continue through the rest of the coherent slice. Conversely, when a defect cannot be isolated safely without execution, use CI rather than guessing.

If a batched CI run finds defects, repair the discovered scoped debt before expanding to another target. Where practical, coalesce related fixes and pay one new validation cycle rather than one cycle per corrected cell.

## Scheduling and delivery order

The default delivery order outside an explicit exception remains **one incomplete pattern at a time**.

During an owner-approved matrix/language scheduling experiment recorded in `docs/roadmap.md`, the delivery unit may instead be a **language-major target slice spanning many incomplete patterns**. This exception changes scheduling and PR ownership boundaries while preserving every pattern-level DoD requirement.

During an approved language-major sweep:

- one active branch/PR may own the target-language slice across many patterns;
- a slow or high-overhead target should normally be set up once and used to implement/validate as many remaining Applicable pattern cells as safely fit that context;
- do **not** open one PR or pay one CI runtime setup per pattern merely to preserve a horizontal work shape;
- partial cells may be tracked in `docs/pattern-sweeps/{target}.md`; that ledger is authoritative for the incomplete slice until final pattern-page reconciliation;
- pattern pages may remain `in-progress` and may lag the full language table during a bounded sweep, but no pattern may reach `validated` until its page itself satisfies the complete canonical structure and reconciles every target/link;
- no partial pattern may be called complete, stable for promotion or roadmap-complete merely because one or several target cells are green;
- `N/A` remains a technical conclusion, never a scheduling shortcut;
- applicability, idiomaticity, behavioral validation, coverage policy, Mermaid, relationships, factual `En Genkidama`, link integrity and comprehension requirements remain unchanged for final pattern completion;
- current `dev`, concurrent lane work and CI must still be reconciled before any stability claim;
- the experiment must remain reversible without invalidating already correct examples.

The roadmap defines the active sweep order and review point. If no active exception exists, the default pattern-major rule applies.

## Content rules

Pattern explanations are authored in Spanish. Code identifiers and canonical pattern names remain in their conventional form.

There is no word-count quota. Prefer the shortest page that completely satisfies the DoD.

Summarize references in original wording; do not reproduce substantial copyrighted passages.

The page and [`wiki/README.md`](../../../wiki/README.md) must not contradict each other. Update the global map in the same increment when a newly verified relationship materially improves it.

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
- [ ] Every current Genkidama language target is classified `Applicable` or `N/A` for this pattern.
- [ ] Every `N/A` classification has a defensible technical justification.
- [ ] **Every Applicable language has at least one verified repository example.**
- [ ] Every implementation link resolves to an existing repository path; no `#` placeholders exist.
- [ ] Each applicable-language example has the strongest reasonable validation evidence available.
- [ ] Where meaningful code-coverage tooling exists, coverage is at least **44%** or a concrete repository-wide exception is documented.
- [ ] Coverage above 72.8% is accepted without penalty; 100% is not required.
- [ ] Three comprehension questions require reasoning.
- [ ] References are present where appropriate and copyright constraints are respected.
- [ ] No `TODO`, `TBD`, `PLACEHOLDER`, empty heading or knowingly speculative claim remains.
- [ ] Markdown links, Mermaid syntax and the final language implementation table have been reviewed.

A pattern with one failed mandatory item remains **in progress**. Do not manufacture low-value tests merely to optimize a metric.

## Review rubric

The Definition of Done is the gate. After the gate passes:

| Dimension | Weight | Review question |
|---|---:|---|
| Correctness of intent and distinctions | 25% | Would an experienced engineer recognize the pattern and its boundaries? |
| Cross-language correctness and idiomaticity | 25% | Do implementations preserve intent without forcing one paradigm onto every language? |
| Decision usefulness | 15% | Can the reader decide when to use or reject it? |
| Relationships and composition | 10% | Does it connect accurately to the surrounding vocabulary? |
| Examples and verification evidence | 15% | Are examples real, linked and proportionately validated? |
| Navigation and references | 10% | Is the page easy to navigate and substantiate? |

Correctness is non-compensable. A materially wrong pattern definition or a non-idiomatic example presented as canonical fails review even if all files exist.

## Agent workflow

1. Read this standard, `docs/roadmap.md`, the relevant target ledger/page, actual example files and the relevant neighborhood of the relationship map.
2. Repay bounded debt in the active owner/slice before expanding it.
3. Outside an active exception, work one pattern at a time. During an owner-approved language-major matrix sweep, work the current target across all remaining patterns authorized by the roadmap.
4. While the same target runtime/toolchain/context is loaded and marginal implementation cost remains low, continue producing coherent cells instead of publishing artificial micro-increments.
5. Prefer staging the coherent slice before moving the review branch/ref, so one publication triggers one expensive CI certification when practical.
6. Never mark a cell verified before its required validation actually passes. A materialized-but-unrun cell stays factual and pending.
7. Keep the target sweep ledger current during matrix work; reconcile each pattern's canonical language table before that pattern is called `validated`.
8. Verify paths and applicability; never infer completion from naming conventions.
9. Update the global relationship map only when a relationship is useful and defensible.
10. Run the strongest practical target validation plus applicable repository checks and inspect the final diff.
11. Once relevant behavior is protected and coverage is >=44%, do not stall delivery merely to chase 72.8%, 90% or 100%.
12. Do not mix course implementation, runtime refactors or unrelated product work in a pattern-sweep PR.

## Approved rollout

1. [`AbstractFactory.md`](../../../wiki/AbstractFactory.md) remains the golden reference for final pattern-page quality.
2. Completed pattern artifacts keep the same full DoD regardless of scheduling strategy.
3. Continue family relationships coherently, but an active owner-approved language-major experiment may traverse the matrix by target runtime to amortize CI.
4. The current experiment order and exit/review point live in `docs/roadmap.md`.
5. Coordinate with the Genkidama Learn lane through the unified repository roadmap: each lane spends roughly 80% of effort on its own delivery and 20% checking compatibility with the other lane.
