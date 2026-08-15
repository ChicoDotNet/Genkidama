# Canonical Design Pattern Authoring Standard

> **Status:** Approved by the owner on 2026-08-14  
> **Applies to:** pattern pages under `wiki/` and their executable examples  
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

### 10. Genkidama usage must be factual

The **En Genkidama** section must either link to a real deliberate use of the pattern and explain why it fits, or state that Genkidama does not currently use it deliberately.

Never distort production architecture to showcase a pattern.

### 11. Tests describe behavior

Verification should test the behavior or variation promised by the pattern, not merely assert class names or inheritance trees.

### 12. Teach distinction

Every page addresses at least one misuse, confusion or over-engineering risk.

## Canonical page structure

Use these headings in this order. A completed page must not leave a required section empty.

~~~markdown
# {Pattern Name}

> **Familia:** {Creational | Structural | Behavioral | Architectural | Integration | Concurrency | Distribution | Presentation | Persistence | Additional}  
> **Intención:** {one-sentence intent}  
> **Estado:** `{in-progress | validated}`  
> **Cobertura de lenguajes:** `{implemented}/{applicable} = {percentage}%`  
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

## Cobertura por lenguaje

The table is authoritative for completion.

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

## Language applicability and coverage rules

### Coverage denominator

For each pattern, build an explicit applicability inventory from the language targets currently maintained by Genkidama. Every target must be classified `Applicable` or `N/A`.

The denominator is the count of languages classified **Applicable**. The numerator is the count of those languages with a verified example satisfying this standard.

A pattern reaches `validated` only when:

```text
implemented_applicable_languages == applicable_languages
coverage == 100%
```

### What counts as an implementation

A language example must:

1. preserve the pattern's intent rather than merely its class diagram;
2. use a reasonable native idiom for that language;
3. live at a stable repository path;
4. be linked from the pattern page;
5. have the strongest practical lightweight validation available for that ecosystem;
6. avoid dependencies or infrastructure that are disproportionate to the teaching goal.

Several languages may use different scenarios if that makes the pattern more natural. Conceptual equivalence matters more than identical examples.

### What counts as `N/A`

`N/A` requires a technical explanation of why the intent itself cannot be represented meaningfully in that language/runtime, not merely why a textbook implementation shape is unavailable.

If the intent can be expressed with functions, modules, messages, closures, prototypes, algebraic types, traits, predicates, macros or other native mechanisms, the language remains **Applicable**.

When uncertain, classify the language as Applicable until review demonstrates otherwise.

### Validation

Do not claim validation that was not executed. When a compiler/runtime cannot reasonably run in CI, document the best available static or structural evidence and the limitation. Missing tooling does not erase the requirement for the example.

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
- [ ] Applicable-language coverage is **100%**.
- [ ] Every implementation link resolves to an existing repository path; no `#` placeholders exist.
- [ ] Each applicable-language example has the strongest reasonable validation evidence available.
- [ ] Three comprehension questions require reasoning.
- [ ] References are present where appropriate and copyright constraints are respected.
- [ ] No `TODO`, `TBD`, `PLACEHOLDER`, empty heading or knowingly speculative claim remains.
- [ ] Markdown links, Mermaid syntax and the coverage table have been reviewed.

A pattern with one failed mandatory item remains **in progress**, regardless of prose quality or how many languages are already implemented.

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

This workflow is active after owner approval.

1. Read this standard, `docs/roadmap.md`, the target page, actual example files and the relevant neighborhood of the relationship map.
2. Repay bounded debt in the target pattern before adding breadth.
3. Work on **one pattern at a time** until its applicable-language coverage reaches 100%.
4. Use **one PR per pattern**. Multiple coherent commits are expected when the cross-language implementation is large; commits may be grouped by language family or validation boundary.
5. Do not start the next pattern while the current pattern is incomplete unless an external technical blocker is documented and no safe work remains on it.
6. Verify paths and applicability; never infer completion from naming conventions.
7. Keep the coverage table current after every increment.
8. Update the global relationship map only when a relationship is useful and defensible.
9. Run applicable repository checks and inspect the final diff.
10. Do not mix course implementation, runtime refactors or unrelated documentation in a pattern PR.

## Approved rollout

1. Retrofit [`AbstractFactory.md`](../../../wiki/AbstractFactory.md) first and make it the golden reference, including 100% Applicable-language coverage.
2. Complete Builder, Factory Method, Prototype and Singleton under the same DoD.
3. Continue family by family so neighboring patterns and their relationships can be reviewed coherently.
4. Coordinate with the Genkidama Learn lane through the unified repository roadmap: each lane spends roughly 80% of effort on its own delivery and 20% checking that its changes remain compatible with the other lane.
