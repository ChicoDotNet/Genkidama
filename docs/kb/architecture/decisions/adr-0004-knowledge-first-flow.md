# ADR-0004: Knowledge-first flow

## Status

Accepted

## Context

Genkidama should not hide architectural intent inside generators only. The roadmap says the knowledge should govern generated code, not the other way around.

## Decision

Use this design order for architectural work: Knowledge Base, then Blueprints, then CLI, then generated applications.

## Consequences

- New generator behavior should be traceable to Knowledge Base guidance.
- Blueprints should express documented standards.
- CLI behavior should expose documented architecture choices.
- Documentation gaps should be treated as architecture gaps, not only writing tasks.

## Follow-up

Future deliveries should update the Knowledge Base when a generator or blueprint changes architectural behavior.
