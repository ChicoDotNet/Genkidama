# Canonical Design Pattern Authoring Standard

> **Status:** Proposed — pending owner approval  
> **Applies to:** pattern pages under `wiki/`  
> **Catalog:** [`wiki/README.md`](../../../wiki/README.md)  
> **Guiding principle:** architecture comes first; a pattern exists in the catalog to explain a real design force, not to force the pattern into production code.

## Purpose

This standard defines the canonical structure and quality bar for every Genkidama Design Pattern page.

A finished page should help a reader answer five questions without memorizing a recipe:

1. **What problem creates pressure for this pattern?**
2. **What trade-off does the pattern make?**
3. **How does it work and how can I recognize it?**
4. **When should I choose something simpler or different?**
5. **Which other patterns commonly appear around it?**

The catalog is a connected design vocabulary. Pattern pages must therefore explain relationships, alternatives and combinations instead of presenting each pattern as an isolated trick.

## Authoring principles

### 1. Problem before pattern

Describe the concrete design problem before naming implementation mechanics. The reader should understand why pressure for the pattern exists before seeing its structure.

### 2. Intent over ceremony

Explain the pattern's intent accurately. Do not define a pattern using the mechanics of a related pattern. For example, Abstract Factory and Factory Method may collaborate, but they are not interchangeable definitions.

### 3. Forces and trade-offs are mandatory

Every pattern resolves competing forces. A page that lists benefits without costs is incomplete.

### 4. Patterns are not prescriptions

Include explicit guidance for **when not to use the pattern**. Prefer a simpler design when it resolves the problem equally well.

### 5. Relationships are first-class content

Every page links back to the global relationship map and explains its most important related patterns. Relationships are described by intent, not just by drawing arrows.

Use these relationship labels consistently:

- **collaborates with** — the patterns commonly solve different parts of the same design;
- **often implemented with** — one pattern commonly supplies a mechanism used by the other;
- **alternative to** — both may address similar pressure with different trade-offs;
- **specializes / generalizes** — one is a more specific or broader expression of the design idea;
- **often confused with** — similar surface structure, different intent.

### 6. Diagrams serve the idea

Every completed page contains at least one Mermaid diagram, but the diagram type must fit the pattern:

- `classDiagram` for participant structure;
- `sequenceDiagram` for collaboration over time;
- `stateDiagram-v2` for state transitions;
- `flowchart` when flow or topology communicates the idea better.

Do not force every pattern into a class diagram.

### 7. Examples must be honest and verifiable

Use one small reference example to make the mechanism concrete, followed by at least one realistic application.

If the repository already contains an implementation, link only to a path that has been verified to exist. Never use `#`, invented paths, or "coming soon" links as implementation links.

A missing implementation is valid information. State that no verified implementation is currently linked instead of pretending one exists.

### 8. Genkidama usage must be factual

The **En Genkidama** section must do one of two things:

- point to a real place where Genkidama deliberately uses the pattern and explain why; or
- state that Genkidama does not currently use it deliberately and explain where it could be appropriate without claiming that it already exists.

The catalog must never distort the production architecture merely to showcase a pattern.

### 9. Tests describe behavior, not class names

Explain how to verify the behavior the pattern promises. Prefer observable contracts, substitutions, state transitions, isolation or collaboration effects over tests that merely assert a particular class hierarchy.

### 10. Teach distinction, not just recognition

The page must identify at least one common misuse, confusion or over-engineering risk. A learner should finish knowing not only what the pattern is, but also what it is **not**.

## Canonical page structure

Use the following headings in this order. A section may be concise, but a completed page must not leave an obligatory heading empty.

~~~markdown
# {Pattern Name}

> **Familia:** {Creational | Structural | Behavioral | Architectural | Integration | Concurrency | Distribution | Presentation | Persistence | Additional}  
> **Intención:** {one-sentence intent}  
> **Estado:** `validated`  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase

{Explain the pattern in plain language in one short paragraph.}

## El problema

{Describe a concrete situation before introducing the solution. Show what becomes coupled, duplicated, fragile, difficult to vary, difficult to coordinate, or otherwise costly.}

## Fuerzas que compiten

- {Force or constraint 1}
- {Force or constraint 2}
- {Force or constraint 3}

## La solución

{Explain the pattern's intent and the key idea that balances those forces. Do not start with framework-specific code.}

## Participantes y responsabilidades

| Participante | Responsabilidad |
|---|---|
| `{Role}` | {What this role knows or does} |

## Cómo funciona

1. {Interaction or decision step 1}
2. {Interaction or decision step 2}
3. {Interaction or decision step 3}

## Diagrama

```mermaid
{Use the Mermaid diagram type that best explains this pattern.}
```

{Explain what the reader should notice in the diagram.}

## Ejemplo mínimo

{Use the smallest example that preserves the pattern's intent. Code may be a verified repository example or clearly marked language-neutral pseudocode. Explain the important lines instead of dumping code.}

## Aplicación real

### {Scenario name}

{Explain the context, why the pattern fits, what it buys, and what a simpler alternative would look like.}

## En Genkidama

{Link to verified Genkidama usage and explain why it is deliberate, OR state honestly that the pattern is not currently used deliberately.}

## Cuándo usarlo

- {Decision signal 1}
- {Decision signal 2}
- {Decision signal 3}

## Cuándo no usarlo

- {Simpler alternative or missing prerequisite 1}
- {Over-engineering risk 2}
- {Context where another pattern is a better fit 3}

## Consecuencias y trade-offs

| A favor | Costo / riesgo |
|---|---|
| {Benefit} | {Cost introduced by the same decision} |

## Patrones relacionados

[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [{Related Pattern}]({RelatedPattern}.md) | {collaborates with / often implemented with / alternative to / specializes / often confused with} | {Intent-level explanation} |

## Errores comunes y confusiones

### {Confusion or misuse}

{Explain why it is tempting, how its intent differs, and how to decide correctly.}

## Cómo comprobar una implementación

- {Observable behavior or contract to test}
- {Substitution or variation that should remain possible}
- {Failure mode or edge case worth testing}

Avoid tests whose only purpose is proving that a particular class name or inheritance tree exists.

## Implementaciones disponibles

List only repository paths that have been verified to exist.

| Lenguaje | Ejemplo | Qué demuestra |
|---|---|---|
| {Language} | [{file name}]({verified relative path}) | {Scenario or mechanism} |

If none are verified, write: **No hay una implementación enlazada y verificada todavía.**

## Comprueba que lo entendiste

1. {Question that requires choosing the pattern from a problem, not recalling a definition}
2. {Question comparing it with a related pattern}
3. {Question about a trade-off or a reason not to use it}

## Resumen

- {Problem pressure}
- {Core design move}
- {Main trade-off}
- {Most important relationship}

## Referencias

- {Primary or authoritative pattern reference}
- {Repository architecture/philosophy reference when relevant}
~~~

## Content rules

### Language

Pattern explanations are currently authored in Spanish. Code identifiers, API names and repository identifiers remain in their native/English form.

Avoid translating established pattern names when doing so would make them harder to search or compare with standard literature. A Spanish explanation may clarify the meaning while retaining the canonical name.

### Length

There is no word-count quota. Prefer the shortest page that fully satisfies the Definition of Done. Repetition added only to make a page look comprehensive is a defect.

### Examples across languages

The historical catalog contains implementations in many languages. The canonical page is **not required to reproduce the same example in every language**.

Instead:

1. keep the conceptual explanation language-neutral;
2. link to existing implementations only after verifying them;
3. prefer a small set of examples that reveal meaningful language differences;
4. treat additional language implementations as expandable evidence, not as a 52 × N mandatory matrix.

### References and attribution

Summarize ideas in original wording. Do not reproduce substantial copyrighted text from books or articles. Prefer primary or authoritative sources when a factual distinction needs support.

### Relationship-map consistency

The page and [`wiki/README.md`](../../../wiki/README.md) must not contradict each other.

When work on a pattern discovers a relationship that materially improves the global map, update the map in the same increment. Do not add every theoretically possible connection: the global graph remains intentionally selective.

## Definition of Done for one pattern page

A pattern page is complete only when **all** of the following are true:

- [ ] The canonical pattern name and family are correct.
- [ ] The one-sentence intent is accurate and not borrowed from a neighboring pattern.
- [ ] The problem is explained before implementation mechanics.
- [ ] At least three meaningful design forces are named.
- [ ] The solution explains intent, not merely class structure.
- [ ] Participants/responsibilities are present when the pattern has identifiable roles; for topology/process patterns, an equivalent responsibility model is used.
- [ ] The interaction or decision flow is explained.
- [ ] At least one Mermaid diagram renders with a diagram type appropriate to the pattern.
- [ ] A minimal example is present and clearly explained.
- [ ] At least one realistic application includes why the pattern fits.
- [ ] Genkidama usage is either verified with a real link or explicitly described as not currently used.
- [ ] `Cuándo usarlo` and `Cuándo no usarlo` are both substantive.
- [ ] Benefits and costs are paired as trade-offs.
- [ ] Important related patterns are linked and their relationship is explained by intent.
- [ ] At least one common confusion, misuse or over-engineering risk is addressed.
- [ ] The testing section describes observable behavior.
- [ ] Every implementation link resolves to an existing repository path; there are no `#` placeholder links.
- [ ] The page includes three comprehension questions that require reasoning.
- [ ] References are present where appropriate and no substantial copyrighted passage is copied.
- [ ] There are no `TODO`, `TBD`, `PLACEHOLDER`, empty headings or knowingly speculative claims.
- [ ] Markdown links and Mermaid syntax have been reviewed.

A page with one failed mandatory item remains **in progress**, regardless of how polished the rest appears.

## Review rubric

The Definition of Done is the gate. This rubric helps reviewers compare quality after the gate passes.

| Dimension | Weight | Review question |
|---|---:|---|
| Correctness of intent and distinctions | 30% | Would an experienced engineer recognize the pattern and its boundaries? |
| Decision usefulness | 20% | Can the reader decide when to use or reject it? |
| Relationships and composition | 15% | Does it connect the pattern to the surrounding vocabulary accurately? |
| Examples and evidence | 15% | Do examples make the mechanism concrete without inventing repository state? |
| Testability and operational consequences | 10% | Does it explain how behavior can be verified? |
| Navigation and references | 10% | Are links real, useful and easy to follow? |

Correctness is non-compensable: a page with a materially wrong definition does not pass even if its total presentation quality is high.

## Recommended agent workflow after approval

This section is intentionally inactive until the owner approves the standard.

For each scheduled increment:

1. Read this standard, the target page, its actual linked source files, and the relevant neighborhood of the relationship map.
2. Repay bounded debt found in the target page before adding breadth.
3. Complete **one pattern per commit**. An increment may complete more than one pattern only if each independently satisfies the Definition of Done.
4. Verify every repository link rather than inferring paths from naming conventions.
5. Update the global relationship map only when the new relationship is both useful and defensible.
6. Run applicable repository checks and inspect the final diff.
7. Keep a family-level PR reviewable; do not mix course work, runtime refactors or unrelated documentation changes.

## Proposed rollout after approval

1. Retrofit [`AbstractFactory.md`](../../../wiki/AbstractFactory.md) first and use it as the **golden reference page**. Its current material is valuable, but its definition and implementation-link presentation should be brought under this standard before copying its shape anywhere else.
2. Complete the rest of the **Creational** family: Builder, Factory Method, Prototype and Singleton.
3. Continue family by family so neighboring patterns can be reviewed together and the relationship map can evolve coherently.
4. Keep the second Genkidama agent focused on the 0 → Junior course program; do not split one scheduled run across both initiatives.

Agent reassignment itself is outside this proposal and requires the owner's explicit go-ahead after the template is approved.
