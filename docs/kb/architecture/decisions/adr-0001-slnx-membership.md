# ADR-0001: Solution format and membership

## Status

Accepted

## Context

A repository project can be referenced by another project and still be missing from `Genkidama.slnx`. That can create different behavior between command line restore, CI and IDE restore.

The repository also needs one active solution entry point for restore, build and test workflows.

## Decision

Use `Genkidama.slnx` as the active solution file.

Projects that participate in the repository build must be listed in `Genkidama.slnx`.

## Consequences

- CI has a stable entry point.
- Local command line restore has a stable entry point.
- New buildable projects update the solution file.
- Referenced projects should not be hidden from the solution.
- Source-only examples can stay outside the solution when documented.

## Follow-up

Add a solution membership check to future project creation work.
