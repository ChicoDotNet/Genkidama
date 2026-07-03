# Architecture Decision Records

This folder records important architecture decisions for Genkidama.

## Format

Each decision should include:

- Status.
- Context.
- Decision.
- Consequences.
- Follow-up work when needed.

## Decisions

- ADR-0001: Use `Genkidama.slnx` as the solution source of truth.
- ADR-0002: Keep Knowledge Base ahead of blueprints and CLI behavior.
- ADR-0003: Prefer built-in primitives and owned abstractions before external runtime packages.

## Rules

- Do not rewrite accepted decisions casually.
- Add a new ADR when the decision changes.
- Keep decision titles stable so future documentation can link to them.
- Record connector-driven naming workarounds when they affect repository structure or public commands.
