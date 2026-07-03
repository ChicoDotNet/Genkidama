# ADR-0003: Runtime dependencies

## Status

Accepted

## Context

The first pod created core runtime surfaces for results, HTTP, data, jobs, events, access and client references.

## Decision

Prefer .NET built-in primitives and Genkidama-owned abstractions before adding external runtime packages.

## Consequences

- The core remains small and easier to teach.
- Optional adapters can be added later without forcing every generated application to carry the same packages.
- New dependencies require explicit justification.

## Follow-up

Provider adapters and optional integrations should remain separate from the core unless promoted by a later decision.
