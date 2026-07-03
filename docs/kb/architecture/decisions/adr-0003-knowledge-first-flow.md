# ADR-0003: Keep Knowledge Base before blueprints and CLI behavior

## Status

Accepted.

## Context

Genkidama should not hide architecture intent inside generators only. The roadmap states that knowledge should govern generated code.

## Decision

Use this order for design work: Knowledge Base, then Blueprints, then CLI, then generated applications.

## Consequences

- New generator behavior should be traceable to Knowledge Base guidance.
- Blueprints should express documented standards.
- CLI behavior should expose documented architecture choices.

## Follow-up

Future deliveries should update the Knowledge Base when a generator or blueprint changes architectural behavior.
