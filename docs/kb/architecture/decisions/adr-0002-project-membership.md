# ADR-0002: Include referenced projects in the solution

## Status

Accepted.

## Context

A project can restore transitively in CI while still causing local IDE restore errors when the referenced project is not loaded through the solution.

## Decision

Any project referenced by a project in `Genkidama.slnx` should also be listed in `Genkidama.slnx`, unless a later decision documents a different rule.

## Consequences

- Visual Studio and command line restore are more likely to agree.
- New source and test projects require solution membership review.
- Connector workarounds that skip solution updates must be revisited.

## Follow-up

Check solution membership during every delivery checklist review.
