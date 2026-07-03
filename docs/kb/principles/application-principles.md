# Generated Application Principles

These principles guide applications produced by Genkidama.

## Contracts first

External boundaries should use explicit contracts. Clients should consume the same contract model as the backend.

## Predictable results

Application operations should return predictable result shapes so clients can handle success, validation, errors and empty results consistently.

## Clear vertical slices

Features should group related contracts, commands, queries, handlers and documentation so a developer can follow a use case end to end.

## Replace infrastructure without rewriting business code

Persistence, notifications, jobs, access control and integrations should be replaceable behind clear abstractions.

## Keep user-facing text ready for localization

Generated applications should separate code identifiers from user-facing text. Future localization should not require renaming source code.

## Make testing natural

Generated code should be easy to test with small unit tests and predictable file or runtime boundaries.

## Prefer boring production quality

Generated code should avoid cleverness. It should be understandable, reviewable and safe to evolve.
