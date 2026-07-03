# ADR-0001: Solution membership

## Status

Accepted

## Context

A repository project can be referenced by another project and still be missing from `Genkidama.slnx`. That can create different behavior between command line restore, CI and IDE restore.

## Decision

Projects that participate in the repository build must be listed in `Genkidama.slnx`.

## Consequences

- New buildable projects update the solution file.
- Referenced projects should not be hidden from the solution.
- Source-only examples can stay outside the solution when documented.

## Follow-up

Add a solution membership check to future project creation work.
