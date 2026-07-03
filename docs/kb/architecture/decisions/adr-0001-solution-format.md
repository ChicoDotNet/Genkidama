# ADR-0001: Use `.slnx` as the active solution format

## Status

Accepted.

## Context

The repository has moved through many generated projects and clients. A single active solution file is needed for CI and local development.

## Decision

Use `Genkidama.slnx` as the active solution file for restore, build and test workflows.

## Consequences

- CI has a stable entry point.
- Local command line restore has a stable entry point.
- New projects should be added to `Genkidama.slnx` when they become part of the active repository build.

## Follow-up

Keep solution membership aligned with project references in every delivery.
